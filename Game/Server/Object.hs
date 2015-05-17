module Game.Server.Object where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import IdentityList

import Game.Shared.Object
import Game.Shared.Types
import Game.Shared.Networking
import Game.Server.Networking

-----------------------
-- Game object types --
-----------------------

-- |Data structure for input to game objects
data ObjInput =  ObjInput {
        oiId :: ILKey -- ^Object id allocated by the engine
       ,oiNetwork :: ServerNetInput -- ^Network input
       ,oiAllObjects :: [GameObject] -- ^List of all game objects
       ,oiCollidingWith :: [GameObject] -- ^List of objects that the object is colliding with
    }

-- |Data structure for the output from game objects
data ObjOutput = ObjOutput {
        ooKillRequest :: Yampa.Event () -- ^Event that is set when this object request to destroy itself
       ,ooSpawnRequests :: Yampa.Event [Object] -- ^Event of list of objects that this object wants to spawn
       ,ooGameObject :: GameObject -- ^Game object structure of how this object wants to represent itself
       ,ooGlobalMessages :: Yampa.Event [NetMessage] -- ^List of messages to send to all connected clients
       ,ooGlobalExceptMessages :: Yampa.Event [(Socket, NetMessage)] -- ^List of messages to send to all clients apart from one specific one
       ,ooMessages :: Yampa.Event [(Socket, NetMessage)] -- ^List of messages to send to specific clients
       ,ooConsole :: String -- ^String to be output to the console
    }
   
-- |Generates a blank game object from the object input   
defaultGameObject objInput = basicGameObject {
        goId = oiId objInput
    }
    
-- |Generates blank object output from the object input
defaultObjOutput objInput = ObjOutput {
        ooKillRequest = Yampa.NoEvent
       ,ooSpawnRequests = Yampa.NoEvent
       ,ooGameObject = defaultGameObject objInput
       ,ooGlobalMessages = Yampa.NoEvent
       ,ooGlobalExceptMessages = Yampa.NoEvent
       ,ooMessages = Yampa.NoEvent
       ,ooConsole = []
    }
 
-- |Type for a game object 
type Object = SF ObjInput ObjOutput