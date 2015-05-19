{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad.Loops

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Data.IORef
import Data.Maybe
import Data.Serialize
import Data.Time

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Concurrency
import Game.Shared.Object
import Game.Shared.Physics
import Game.Shared.Arrows
import Game.Server.Object
import Game.Server.Networking
import Game.Server.Objects.GameSetup

import IdentityList

-- |Gets the number of ticks since the server was started
getTicks :: IO Double
getTicks = do
    now <- getCurrentTime
    let dayTime = utctDayTime now
        rationalDayTime = toRational dayTime
        integralVal = fromRational rationalDayTime
        integerVal = integralVal
    return integralVal

--
-- Based on code at: 
-- http://lambdor.net/?p=59
--

-- |Entrypoint function
main :: IO ()
main = do
    tickRef <- newIORef =<< getTicks
    sock <- createServerSocket
    msgs <- initMVar :: IO (MVar ServerNetInput)
    conns <- initMVar :: IO (MVar [Socket])
    reactimate (initialise tickRef sock msgs conns) (sense tickRef sock msgs) (actuate sock conns) (process objs)
    where                  
        objs = listToIL [setupBasicGame] 

-- |Initialises the client environment and sets up SDL
initialise :: IORef Double -- ^The IOReference to the tick counter
           -> Socket -- ^The server socket
           -> MVar ServerNetInput -- ^The MVar referencing inbound network events 
           -> MVar [Socket] -- ^The MVar referencing all client connections
           -> IO ServerNetInput -- ^Initial network input
initialise tickRef sock msgs conns = do    
        -- Start listening
        forkIO (serverAcceptLoop sock msgs conns)
        -- Store the current time
        t <- getTicks
        writeIORef tickRef t
        return []
        
-- |Performs input handling for the reactimate loop, returning both
-- the number of second since the last call and all network input events.
sense :: IORef Double -- ^The IOReference to the tick counter
      -> Socket -- ^The server socket
      -> MVar ServerNetInput -- ^The MVar referencing inbound network events
      -> Bool -- ^Unused
      -> IO (DTime, Maybe ServerNetInput) -- ^Number of seconds since last call and the network input
sense tickRef sock msgs _ = do    
    -- Compute the time difference between now and the last frame
    t <- getTicks
    t' <- readIORef tickRef
    -- Grab all of the network events
    netEvents <- popAllMVar msgs
    -- Wrute the cyrrebt tune and return the time detla
    writeIORef tickRef t
    let dt = t - t'
    return (dt, Just netEvents)
    
-- |Performs output handling for the reactimate loop, drawing the result
-- of the objects to the screen, and sending all output messages to the clients
actuate :: Socket -- ^The server socket
        -> MVar [Socket] -- ^The MVar referencing all client connections
        -> Bool -- ^Unused
        -> IL ObjOutput -- ^Object output identity list
        -> IO Bool -- ^True if the server should shutdown
actuate sock conns _ oos = do
    -- Send any packets to the clients (ignoring empty lists)
    let objOutputs = elemsIL oos
    allConns <- readMVar conns
    mapM_ (\oo -> mapM_ (\msg -> mapM_ (\sock -> sendMessage sock msg) allConns) (eventToList (ooGlobalMessages oo))) objOutputs
    mapM_ (\oo -> mapM_ (\(notSock, msg) -> mapM_ (\sock -> if sock /= notSock then sendMessage sock msg else return ()) allConns) (eventToList (ooGlobalExceptMessages oo))) objOutputs
    mapM_ (\oo -> mapM_ (\(sock, msg) -> sendMessage sock msg) (eventToList (ooMessages oo))) objOutputs
    -- Output console messages
    mapM_ (\oo -> let con = ooConsole oo in if (not . null) con then putStrLn con else return ()) objOutputs
    return $ null (keysIL oos)
    
-- |Performs the game message pump, passing the inputs into each
-- game object, producing a list of the object outputs
process :: IL Object -- ^Identity list of all the object signal functions
        -> SF ServerNetInput (IL ObjOutput)
process objs = proc input -> do
    rec
        objOut <- core objs -< (input, objOut)
    returnA -< objOut

-- |Performs each object in turn, and executes the killAndSpawn fuction
-- when finished
core :: IL Object -- ^Identity list of all the object signal functions
     -> SF (ServerNetInput, IL ObjOutput) (IL ObjOutput)
core objs = dpSwitch route objs (arr killAndSpawn >>> notYet) (\sfs f -> core (f sfs))

-- |Routes all events to each object
route :: (ServerNetInput, IL ObjOutput) -- ^Network input
      -> IL sf -- ^Identity list of all the object signal functions
      -> IL (ObjInput, sf)  -- ^List of inputs for each signal function
route (messages, oos) objs = mapIL route' objs
    where
        route' (k, obj) = (ObjInput {
                oiId = k,
                oiNetwork = messages,
                oiAllObjects = allObjects,
                oiCollidingWith = fmap snd (filter (\(a, _) -> (goId a) == k) allCollisions)
            }, obj)       
        allObjects = elemsIL (fmap ooGameObject oos)
        allCollisions = findCollisions allObjects

-- |Reads the output from all the objects, performing their output kill requests and
-- output spawn requests
killAndSpawn :: ((ServerNetInput, IL ObjOutput), IL ObjOutput) -- ^Network input and the list of object outputs 
             -> Yampa.Event (IL Object -> IL Object)
killAndSpawn ((msgs, _), oos) = foldl (mergeBy (.)) noEvent events
    where
        events = [mergeBy (.) (ooKillRequest oo `tag` (deleteIL k))
                              (fmap  (foldl (.) id . map insertIL_) (ooSpawnRequests oo))
                 | (k, oo) <- assocsIL oos]
