{-# LANGUAGE Arrows #-}

module Game.Client.Networking where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.Exit

import Control.Concurrent.MVar
import Control.Exception

import Data.Serialize

import IdentityList

import Game.Shared.Serialization
import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Concurrency
import Game.Shared.Arrows
import Game.Shared.Object

--------------------
-- Network events --
--------------------

-- |Signal function that fires an event when the client sucessfully connects to the server
connectionSuccess :: SF NetworkInput (Yampa.Event ())
connectionSuccess = edge <<< arr (elem ConnectionSuccess)

-- |Signal function that fires an even when the client fails to connect to the server
connectionFailed :: SF NetworkInput (Yampa.Event ())
connectionFailed = edge <<< arr (elem ConnectionFailed)

-- |Signal function that reads all messages from the network input events
inboundMessages :: SF NetworkInput [NetMessage]
inboundMessages = arr (map fromEvent . filter isMessage)
    where
        fromEvent (Message msg) = msg
        isMessage (Message msg) = True
        isMessage _ = False

-- |Signal function that listens for movement events for a certain game object
objectMoved :: ILKey -- ^Id of the object to listen for movement events for
            -> SF NetworkInput (Yampa.Event Vector2D)
objectMoved objId = proc netInput -> do
    msgs <- inboundMessages -< netInput
    moveMsgs <- arr (filter isMoveMessage) -< msgs
    returnA -< if (not . null) moveMsgs
        then Yampa.Event ((\(Move _ p) -> p) (last moveMsgs))
        else noEvent
    where
        isMoveMessage (Move id _) = id == objId
        isMoveMessage _ = False
        
-- |Signal function that listens for stat update events for a certain stat of a certain game objects
objectStatUpdated :: ILKey -- ^Id of the object to listen for stat update events for
                  -> StatName -- ^Name of the stat to listen for update events for
                  -> SF NetworkInput (Yampa.Event Int)
objectStatUpdated objId statName = proc netInput -> do
    msgs <- inboundMessages -< netInput
    statMessages <- arr (filter isStatUpdateMessage) -< msgs
    returnA -< if (not . null) statMessages
        then Yampa.Event ((\(UpdateStats _ _ value) -> value) (last statMessages))
        else noEvent
    where
        isStatUpdateMessage (UpdateStats id name _) = (id == objId) && (name == statName)
        isStatUpdateMessage _ = False

-- |Signal function that listens for player data messages
playerData :: SF NetworkInput (Yampa.Event (ILKey, GameTeam, PlayerClass))
playerData = proc netInput -> do
    msgs <- inboundMessages -< netInput
    playerDataMsgs <- arr (filter isPlayerDataMessage) -< msgs
    returnA -< if (not . null) playerDataMsgs
        then Yampa.Event ((\(PlayerData id team playerClass) -> (id, team, playerClass)) (last playerDataMsgs))
        else noEvent
    where
        isPlayerDataMessage (PlayerData _ _ _) = True
        isPlayerDataMessage _ = False
        
-- |Signal function that listens for a kill even for a certain game object
killedObject :: ILKey -- ^Id of the object to listen for kill events for
             -> SF NetworkInput (Yampa.Event ())
killedObject objId = proc netInput -> do
    msgs <- inboundMessages -< netInput
    killMsgs <- arr (filter isKillMessage) -< msgs
    event <- arr (condEvent) -< (not . null) killMsgs
    returnA -< event
    where
        isKillMessage (Kill id) = id == objId
        isKillMessage _ = False
        
-- |Signal function that listens for object creation events
createObject :: ObjectType -- ^Object type to listen for creation events of
             -> SF NetworkInput (Yampa.Event [GameObject])
createObject objType = proc netInput -> do
    msgs <- inboundMessages -< netInput
    createMsgs <- arr (filter isCreateMessage) -< msgs
    objIds <- arr (map (\(Create obj) -> obj)) -< createMsgs
    returnA -< listToEvent objIds
    where
        isCreateMessage (Create obj) = (goType obj) == objType
        isCreateMessage _ = False
        
-- |Signal function that listens for destroy events for a certain object
destroyObject :: ILKey -- ^Object to listen for destruction events for
              -> SF NetworkInput (Yampa.Event ())
destroyObject objId = proc netInput -> do
    msgs <- inboundMessages -< netInput
    destroyMsgs <- arr (filter isDestroyMessage) -< msgs
    event <- arr (condEvent) -< (not . null) destroyMsgs
    returnA -< event
    where
        isDestroyMessage (Destroy id) = id == objId
        isDestroyMessage _ = False
        
-- |Signal function that waits for a game over network message
teamWon :: SF NetworkInput (Yampa.Event GameTeam) -- ^Output contains the winning team
teamWon = proc netInput -> do
    msgs <- inboundMessages -< netInput
    teamWonMsgs <- arr (filter isTeamWonMessage) -< msgs
    returnA -< if (not . null) teamWonMsgs
        then Yampa.Event ((\(TeamWon team) -> team) (last teamWonMsgs))
        else noEvent
    where
        isTeamWonMessage (TeamWon _) = True
        isTeamWonMessage _ = False

-----------------------
-- Network functions --
-----------------------
-- |Sends a message to a specific client socket
sendMessage :: Socket -- ^Client to send message too
            -> NetMessage -- ^The message to send
            -> IO ()
sendMessage sock msg = withSocketsDo $ do
    sendAll sock (encode msg)
    return ()

-- |Creates the client socket object
createClientSocket :: IO Socket
createClientSocket = withSocketsDo $ do
    proto <- getProtocolNumber "tcp"
    sock <- socket AF_INET Stream proto
    return sock

-- |The client network loop, connects then listens for messages
clientNetworkLoop :: Socket -- ^The client socket
                  -> MVar NetworkInput -- ^The message list MVar
                  -> IO ()   
clientNetworkLoop sock msgs = withSocketsDo $  do   
    address <- inet_addr "127.0.0.1"
    res <- try (connect sock (SockAddrInet 5050 address)) :: IO (Either SomeException ())
    case res of
        Left _ -> pushMVar msgs ConnectionFailed
        Right _ -> do
            pushMVar msgs ConnectionSuccess
            clientListenLoop sock msgs
    
-- |Listens for messages, pushing them onto the MVar until we 
-- disconnect from the server.   
clientListenLoop :: Socket -- ^The client socket
                 -> MVar NetworkInput -- ^The message list MVar 
                 -> IO ()    
clientListenLoop sock msgs = withSocketsDo $ do
    recvd <- recv sock 16384
    case decodeAll recvd :: Either String [NetMessage] of
        Left str -> do
            sClose sock
            pushMVar msgs Disconnected
            error ("Bad message: " ++ str ++ " : " ++ show recvd)
            exitFailure
        Right decodedMsgs -> do
            pushAllMVar msgs (map (Message) decodedMsgs)
            clientListenLoop sock msgs
