module Game.Client.Object where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import IdentityList

import Game.Shared.Object
import Game.Shared.Types
import Game.Shared.Networking
import Game.Client.Graphics
import Game.Client.Input

-----------------------
-- Game object types --
-----------------------

-- |Data structure for input to game objects
data ObjInput =  ObjInput {
        oiId :: ILKey -- ^Object id allocated by the engine
       ,oiCamera :: Vector2D -- ^Current camera position
       ,oiInput :: PlayerInput -- ^Player input from SDL
       ,oiNetwork :: NetworkInput -- ^Network input
       ,oiAllObjects :: [GameObject] -- ^List of all game objects
       ,oiCollidingWith :: [GameObject] -- ^List of objects that the object is colliding with
    }

-- |Data structure for the output from game objects
data ObjOutput = ObjOutput {
        ooCameraDelta :: Vector2D -- ^Amount to move the camera by
       ,ooCamera :: Vector2D -- ^The position of the camera
       ,ooKillRequest :: Yampa.Event () -- ^Event that is set when this object request to destroy itself
       ,ooSpawnRequests :: Yampa.Event [Object] -- ^Event of list of objects that this object wants to spawn
       ,ooGameObject :: GameObject -- ^Game object structure of how this object wants to represent itself
       ,ooGraphic :: Graphic -- ^Graphic object that the object wants to draw
       ,ooGraphicLayer :: GraphicLayer -- ^The layer to draw the graphic on
       ,ooMessages :: Yampa.Event [NetMessage] -- ^List of messages to be sent to the server
    }

-- |Generates a blank game object from the object input  
defaultGameObject objInput = basicGameObject {
        goId = oiId objInput
    }
    
-- |Generates blank object output from the object input
defaultObjOutput objInput = ObjOutput {
        ooCameraDelta = zeroVector
       ,ooCamera = oiCamera objInput
       ,ooKillRequest = Yampa.NoEvent
       ,ooSpawnRequests = Yampa.NoEvent
       ,ooGameObject = defaultGameObject objInput
       ,ooGraphic = emptyG
       ,ooGraphicLayer = GameLayer 0
       ,ooMessages = Yampa.NoEvent
    }
    
-- |Type for a game object 
type Object = SF ObjInput ObjOutput