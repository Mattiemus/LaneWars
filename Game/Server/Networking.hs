{-# LANGUAGE Arrows #-}

module Game.Server.Networking where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

import Data.Serialize
import Data.ByteString.Internal

import Game.Shared.Serialization
import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Concurrency
import Game.Shared.Arrows

-----------------
-- Event types --
-----------------

-- |Type alias for server network input
type ServerNetInput = [(Socket, NetEvent)]
            
--------------------
-- Network events --
--------------------

-- |Signal function that fires an event when a client has connected
clientsConnected :: SF ServerNetInput (Yampa.Event [Socket])
clientsConnected = proc netInput -> do
    newClients <- arr (map fst . filter isNewConnection) -< netInput
    returnA -< listToEvent newClients
    where
        isNewConnection (_, ConnectionSuccess) = True
        isNewConnection _ = False

-- |Signal function that fires an event when a specified client disconnects
clientDisconnected :: Socket -- ^The client to listen for disconnect events for
                   -> SF ServerNetInput (Yampa.Event ())
clientDisconnected sock = edge <<< arr (elem (sock, Disconnected))

-- |Signal function that unpacks all events from a specific socket
eventsFromSock :: Socket -> SF ServerNetInput [NetEvent]
eventsFromSock sock = arr (map snd . filter isFromSocket)
    where
        isFromSocket (s, _) = (s == sock)
        
-- |Signal function that unpacks all message events for a specific socket
messagesFromSock :: Socket -> SF ServerNetInput [NetMessage]
messagesFromSock sock = arr (map fromEvent . filter isMessage) <<< eventsFromSock sock
    where
        fromEvent (Message msg) = msg
        isMessage (Message msg) = True
        isMessage _ = False
        
-- |Signal function that fires an event when a client sends a move request message
moveRequest :: Socket -> SF ServerNetInput (Yampa.Event Vector2D)
moveRequest sock = proc netInput -> do
    msgs <- messagesFromSock sock -< netInput
    moveMsgs <- arr (filter isMoveMessage) -< msgs
    returnA -< if (not . null) moveMsgs
        then Yampa.Event ((\(MoveMe p) -> p) (last moveMsgs))
        else noEvent
    where
        isMoveMessage (MoveMe _) = True
        isMoveMessage _ = False
 
-- |Signal function that fires an event when a client sends a attack request message 
attackRequest :: Socket -> SF ServerNetInput (Yampa.Event (Int, Vector2D))
attackRequest sock = proc netInput -> do
    msgs <- messagesFromSock sock -< netInput
    attackMsgs <- arr (filter isAttackMessage) -< msgs
    returnA -< if (not . null) attackMsgs
        then Yampa.Event ((\(PlayerAttack id dir) -> (id, dir)) (last attackMsgs))
        else noEvent
    where
        isAttackMessage (PlayerAttack _ _) = True
        isAttackMessage _ = False
        
-----------------------
-- Network functions --
-----------------------
-- |Sends a message to a specific client socket
sendMessage :: Socket -- ^Socket of the client to send the message too
            -> NetMessage -- ^Message to send
            -> IO ()
sendMessage sock msg = withSocketsDo $ do
    try (sendAll sock (encode msg)) :: IO (Either SomeException ())
    return ()

-- |Creates the server socket object
createServerSocket :: IO Socket
createServerSocket = withSocketsDo $ do
    proto <- getProtocolNumber "tcp"
    sock <- socket AF_INET Stream proto
    bindSocket sock (SockAddrInet 5050 iNADDR_ANY)
    listen sock 10
    putStrLn "Listening on port: 5050"
    return sock
    
-- |Listen for new connections
serverAcceptLoop :: Socket -- ^Server socket
                 -> MVar ServerNetInput -- ^MVar of receieved network messages
                 -> MVar [Socket] -- ^MVar of all connected clients
                 -> IO ()
serverAcceptLoop sock msgs conns = withSocketsDo $ do
    (clientSock, clientAddr) <- accept sock
    putStrLn ("New connection from: " ++ show clientAddr)
    pushMVar conns clientSock
    pushMVar msgs (clientSock, ConnectionSuccess)
    forkIO (serverClientListenLoop clientSock clientAddr msgs conns)
    serverAcceptLoop sock msgs conns
    
-- |Client connection loop
serverClientListenLoop :: Socket -- ^Client socket
                       -> SockAddr -- ^Remote endpoint of the client
                       -> MVar ServerNetInput -- ^MVar of receieved network messages
                       -> MVar [Socket] -- ^MVar of all connected clients
                       -> IO ()
serverClientListenLoop clientSock clientAddr msgs conns = withSocketsDo $ do
    tryRecv <- try (recv clientSock 65536) :: IO (Either SomeException ByteString)
    case tryRecv of 
        Left err -> do 
            putStrLn ("Lost connection from: " ++ show clientAddr ++ " (" ++ show err ++ ")")
            removeFromMVar conns clientSock
            pushMVar msgs (clientSock, Disconnected)
            sClose clientSock
        Right msg -> do
            case decodeAll msg :: Either String [NetMessage] of
                Left str -> do
                    putStrLn ("Bad message from: " ++ show clientAddr ++ ": " ++ str)
                    removeFromMVar conns clientSock
                    pushMVar msgs (clientSock, Disconnected)
                    sClose clientSock
                    serverClientListenLoop clientSock clientAddr msgs conns
                Right decodedMsgs -> do
                    pushAllMVar msgs (map (\m -> (clientSock, Message m)) decodedMsgs)
                    serverClientListenLoop clientSock clientAddr msgs conns
