{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad.Loops

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Data.IORef
import Data.Maybe
import Data.Serialize

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

import GHC.Word
import GHC.Exts

import IdentityList

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Concurrency
import Game.Shared.Object
import Game.Shared.Physics
import Game.Shared.Arrows
import Game.Client.Objects.Network
import Game.Client.Objects.Input
import Game.Client.Objects.UI
import Game.Client.Object
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

--
-- Based on code at: 
-- http://lambdor.net/?p=59
--

-- |Entrypoint to the client
main :: IO ()
main = do
    tickRef <- newIORef =<< SDL.getTicks
    sock <- createClientSocket
    msgs <- initMVar :: IO (MVar NetworkInput)
    reactimate (initialise tickRef sock msgs) (sense tickRef sock msgs) (actuate sock) (process zeroVector objs)
    SDL.quit
    where                 
        objs = listToIL [mapBackground, netManager, cameraManager, uiManager]   

-- |Initialises the client environment, creating the game window 
-- and sets up SDL
initialise :: IORef Word32 -- ^The IOReference to the tick counter
           -> Socket -- ^The client socket 
           -> MVar NetworkInput -- ^The MVar referencing inbound network events
           -> IO (PlayerInput, NetworkInput) -- ^The initial player and network input data
initialise tickRef sock msgs = do
        -- Setup SDL
        SDL.init [SDL.InitVideo]
        screen <- SDL.setVideoMode windowWidth windowHeight windowDepth [SDL.HWSurface]
        SDL.setCaption windowCaption []     
        -- Start listening
        forkIO (clientNetworkLoop sock msgs)        
        -- Gather up events and return them
        events <- unfoldWhileM (/= SDL.NoEvent) SDL.pollEvent
        t <- SDL.getTicks
        writeIORef tickRef t
        return (events, [])
    where
        windowWidth = 1024  
        windowHeight = 768
        windowDepth = 32
        windowCaption = "Lane Wars"  
        
-- |Performs input handling for the reactimate loop, returning both
-- the number of second since the last cal, and a tuple containing
-- all player input events and all the network input messages.
sense :: IORef Word32 -- ^The IOReference to the tick counter
      -> Socket -- ^The client socket
      -> MVar NetworkInput -- ^The MVar referencing inbound network events
      -> Bool -- ^Unused
      -> IO (DTime, Maybe (PlayerInput, NetworkInput)) -- ^The number of seconds since the last call, player input, and network input
sense tickRef sock msgs _ = do
    -- Compute the time difference between now and the last frame
    t <- SDL.getTicks
    t' <- readIORef tickRef    
    -- Grab all of the network events
    netEvents <- popAllMVar msgs
    -- Return all the events passed since the last frame
    events <- unfoldWhileM (/= SDL.NoEvent) SDL.pollEvent    
    -- Write the current time and return the time delta
    writeIORef tickRef t
    let dt = (0x100000000 - (fromIntegral (t' - t))) / 1000
    return (dt, Just (events, netEvents))
    where
        isMessage (Message _) = True
        isMessage _ = False
    
-- |Performs output handling for the reactimate loop, drawing the result
-- of the objects to the screen, and sends all output messages to the server
actuate :: Socket -- ^The client socket
        -> Bool -- ^Unused
        -> IL ObjOutput -- ^Object output identity list
        -> IO Bool -- ^True if we are to exit
actuate sock _ oos = do
    -- Send any packets to the server (ignoring empty lists)
    mapM_ (\oo -> mapM_ (\msg -> sendMessage sock msg) (eventToList (ooMessages oo))) (elemsIL oos) 
    -- Clear the screen
    screen <- SDL.getVideoSurface
    SDL.fillRect screen Nothing (SDL.Pixel 0x00FFFFFF)  
    -- Render each object to the screen
    let camPos = ooCamera (head (elemsIL oos))
        camX = round (vector2X camPos)
        camY = round (vector2Y camPos)
        drawObject screen camX camY obj = case ooGraphicLayer obj of
            UILayer -> paintGraphic (ooGraphic obj) screen 0 0
            _ -> if isWithin (ooGameObject obj) camPos (vector2 1024 768)
                    then paintGraphic (ooGraphic obj) screen camX camY
                    else return ()
    mapM_ (drawObject screen camX camY) (sortWith gfxSorter (elemsIL oos))
    SDL.flip screen
    return $ null (keysIL oos)    
    where 
        gfxSorter obj = case ooGraphicLayer obj of
            GameLayer x -> x
            UILayer -> 9999999999

-- |Performs the game message pump, passing the inputs into each
-- game object then producing a list of the object outputs
process :: Vector2D -- ^Current position of the camera
        -> IL Object -- ^Identity list of all the object signal functions
        -> SF (PlayerInput, NetworkInput) (IL ObjOutput)
process cam objs = proc input -> do
    rec
        objOut <- core cam objs -< (input, objOut)
    returnA -< objOut

-- |Performs each object in turn and executes the killAndSpawn fuction
-- when finished
core :: Vector2D -- ^Current position of the camera
     -> IL Object -- ^Identity list of all the object signal functions
     -> SF ((PlayerInput, NetworkInput), IL ObjOutput) (IL ObjOutput)
core cam objs = dpSwitch (route cam) objs (arr (killAndSpawn cam) >>> notYet) (\sfs (camNew, f) -> core camNew (f sfs))

-- |Routes all events to each object
route :: Vector2D -- ^Current camera position
      -> ((PlayerInput, NetworkInput), IL ObjOutput) -- ^Input from sense as well as the list of game objects
      -> IL sf -- ^Identity list of all the object signal functions
      -> IL (ObjInput, sf) -- ^List of the objects to be run and the inputs genererated for each
route cam ((input, messages), oos) objs = mapIL route' objs
    where
        route' (k, obj) = (ObjInput {
                oiId = k,
                oiCamera = cam,
                oiInput = input,
                oiNetwork = messages,
                oiAllObjects = allObjects,
                oiCollidingWith = fmap snd (filter (\(a, _) -> (goId a) == k) allCollisions)
            }, obj)       
        allObjects = elemsIL (fmap ooGameObject oos)
        allCollisions = findCollisions allObjects
        
-- |Reads the output from all the objects performing their output kill requests and
-- output spawn requests
killAndSpawn :: Vector2D -- ^current camera position
             -> (((PlayerInput, NetworkInput), IL ObjOutput), IL ObjOutput) -- ^The outputs from the game objects as well as the inputs that generated them
             -> Yampa.Event (Vector2D, IL Object -> IL Object) -- ^An event containing the new camera position and a function to modify the game objects list
killAndSpawn cam (((input, messages), _), oos) = if any checkEscKey input || Quit `elem` input
                                    then Yampa.Event (cameraDelta, \_ -> emptyIL)
                                    else case (isEvent cameraMovedEvent, isEvent mergedEvents) of
                                            (False, False) -> noEvent
                                            (True, False) -> attach cameraMovedEvent (id)
                                            (False, True) -> joinE (Yampa.Event cam) mergedEvents
                                            (True, True) -> joinE cameraMovedEvent mergedEvents
    where
        cameraDelta = foldl (^+^) zeroVector (map ooCameraDelta (elemsIL oos))
        cameraMovedEvent = taggedCondEvent (cameraDelta /= zeroVector) (cam ^+^ cameraDelta)
        mergedEvents = foldl (mergeBy (.)) noEvent events
        events = [mergeBy (.) (ooKillRequest oo `tag` (deleteIL k))
                              (fmap  (foldl (.) id . map insertIL_) (ooSpawnRequests oo))
                 | (k, oo) <- assocsIL oos]
        checkEscKey (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE  _ _)) = True
        checkEscKey _ = False
        