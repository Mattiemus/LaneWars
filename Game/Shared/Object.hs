{-# LANGUAGE DeriveGeneric #-}

module Game.Shared.Object where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import GHC.Generics
import GHC.Exts

import IdentityList

import Data.Serialize
import Data.List

import Game.Shared.Types
import Game.Shared.Physics

--------------------------------
-- Game object list functions --
--------------------------------
-- |Searches through a list of objects for the first object with the specified id
findObjById :: ILKey -- ^Object id to search for
               -> [GameObject] -- ^List of objects to search through
               -> Maybe GameObject -- ^Either the first object with the specified id, or no value
findObjById id = find (\obj -> (goId obj) == id)

-- |Searches through a list of objects for the first object in the specified team
findObjByTeam :: GameTeam -- ^Team to search for
              -> [GameObject] -- ^List of objects to search through
              -> Maybe GameObject -- ^Either the first object with the specified team association, or no value
findObjByTeam team = find (\obj -> (goTeam obj) == team)

-- |Searches through a list of objects for the first object with the specified type
findObjByType :: ObjectType -- ^Object type to search for
              -> [GameObject] -- ^List of objects to search through
              -> Maybe GameObject -- ^Either the first object with the specified type, or no value
findObjByType ty = find (\obj -> (goType obj) == ty)

-- |Search for objects that are colliding
findCollisions :: [GameObject] -- ^List of game objects to search through
               -> [(GameObject, GameObject)] -- ^List of pairs of objects that are colliding
findCollisions objs = [(aObj, bObj) | aObj <- objs, bObj <- objs, areColliding aObj bObj]

-- |Searches for objects that are within the specified radius around a specific object
findWithinRangeOf :: GameObject -- ^Game about to search around
                  -> Double  -- ^Maximum range radius
                  -> [GameObject] -- ^List of objects to search through
                  -> [GameObject] -- ^List of objects within the specified area
findWithinRangeOf obj range objs = filter (\x -> vectorDistance (goPos obj) (goPos x) <= range) objs
        
-- |Searches for objects that are within the specified radius around the object with the 
-- specified id
findWithinRangeofId :: ILKey -- ^ID of the object to search around
                    -> Double -- ^Maximum range radius
                    -> [GameObject] -- ^List of objects to search through
                    -> [GameObject] -- ^List of objects within the specified area
findWithinRangeofId objId range objs = case findObjById objId objs of
    Nothing -> []
    Just obj -> filter (\x -> vectorDistance (goPos obj) (goPos x) <= range) objs
    
-- |Searches for objects within the specified radius around a specific point
findWithinRangeOfPoint :: Vector2D -- ^Point to search around
                       -> Double -- ^Maximum range radius
                       -> [GameObject] -- ^List of objects to search through
                       -> [GameObject] -- ^List of objects within the specified area
findWithinRangeOfPoint pos range objs = filter (\x -> vectorDistance pos (goPos x) <= range) objs

-------------------------------------
-- Game object filtering functions --
-------------------------------------
-- |Removes all game objects that are not of a specific type
filterObjByType :: [ObjectType] -- ^List of object types to keep
                -> [GameObject] -- ^List of game objects to filter
                -> [GameObject] -- ^List of game objects that are one of the specified types
filterObjByType types objs = filter (\obj -> (goType obj) `elem` types) objs

-- |Removes all game objects that are not in the specified team
filterObjByTeam :: GameTeam -- ^Team of objects to keep
                -> [GameObject] -- ^List of game objects to filter
                -> [GameObject] -- ^List of game objects within the specified team
filterObjByTeam team objs = filter (\obj -> (goTeam obj) == team) objs
    
-----------------------------------
-- Game object sorting functions --
-----------------------------------
-- |Sorts a list of game objects by distance from a specific object
sortByDistanceFrom :: GameObject -- ^Object to act as the center point
                   -> [GameObject] -- ^List of objects
                   -> [GameObject] -- ^List of objects now ordered by there distance from the central object
sortByDistanceFrom obj objs = sortWith (\x -> vectorDistance objPos (goPos x)) objs
    where
        objPos = goPos obj
        
-- |Sorts a list of game objects by distance from a specific point
sortByDistanceFromPoint :: Vector2D -- ^Central point
                        -> [GameObject] -- ^List of objects
                        -> [GameObject] -- ^List of objects now ordered by there distance from the central point
sortByDistanceFromPoint pos objs = sortWith (\x -> vectorDistance pos (goPos x)) objs

----------------------------
-- Game object predicates --
----------------------------
-- |Tests if two objects are colliding
areColliding :: GameObject -> GameObject -> Bool
areColliding a b = ((withinRange ax bx (bx + bwidth)) || (withinRange bx ax (ax + awidth)))
    && ((withinRange ay by (by + bheight)) || (withinRange by ay (ay + aheight)))
    where 
        ax = vector2X (goPos a)
        ay = vector2Y (goPos a)
        awidth = vector2X (goSize a)
        aheight = vector2Y (goSize a)
        bx = vector2X (goPos b)
        by = vector2Y (goPos b)
        bwidth = vector2X (goSize b)
        bheight = vector2Y (goSize b)
        
-- |Tests if an object is within a specific area
isWithin :: GameObject -- ^Object to test
         -> Vector2D -- ^Area start coordinates
         -> Vector2D -- ^Area width and height
         -> Bool
isWithin obj pos size = ((withinRange ax bx (bx + bwidth)) || (withinRange bx ax (ax + awidth)))
    && ((withinRange ay by (by + bheight)) || (withinRange by ay (ay + aheight)))
    where 
        ax = vector2X (goPos obj)
        ay = vector2Y (goPos obj)
        awidth = vector2X (goSize obj)
        aheight = vector2Y (goSize obj)
        bx = vector2X pos
        by = vector2Y pos
        bwidth = vector2X size
        bheight = vector2Y size
        
---------------------------
-- Game object functions --
---------------------------
-- |Computes the center point of an object
middleOfObject :: GameObject -> Vector2D
middleOfObject obj = pos ^+^ halfSize
    where
        pos = goPos obj
        size = goSize obj
        halfSize = size ^/ 2

--------------------------------
-- Game object representation --
--------------------------------
-- |Enumeration of object types
data ObjectType = NotAnObject -- ^Game object of another type, i.e. the camera manager
                | Player -- ^Player game object
                | ControlledPlayer -- ^Local player game object
                | PlayerProjectile Int -- ^A player spawned projectile
                | Minion -- ^A minion object
                | MinionProjectile -- ^A minion spawnwed projectile
                | Turret -- ^A turret object
                | TurretProjectile -- ^A turret spawned projectile
                | Nexus -- ^A teams base
    deriving(Show, Eq, Generic)
instance Serialize ObjectType
    
-- |Enumeration of teams
data GameTeam = Neutral -- ^Neither a member of the red or blue team
              | Red
              | Blue
    deriving(Show, Eq, Generic)
instance Serialize GameTeam

-- |Computes the enemy team of a specified game team
enemyTeam :: GameTeam -> GameTeam
enemyTeam Red = Blue
enemyTeam Blue = Red

-- |Enumeration of all possible player classes
data PlayerClass = Mage
    deriving(Show, Eq, Generic)
instance Serialize PlayerClass

-- |Enumeration of the three map lanes
data MapLane = Top
             | Bottom
             | Middle
    deriving(Show, Eq, Generic)
instance Serialize MapLane

-- |Structure of the object stats
data Stats = Stats {
        stHealth :: Int,
        stMaxHealth :: Int,
        stMana :: Int,
        stMaxMana :: Int,
        stSpeed :: Int,
        stDefence :: Int,
        stAttack :: Int
    }
    deriving(Show, Eq, Generic)
instance Serialize Stats

-- |Enumeration of stat names
data StatName = Health | MaxHealth
              | Mana | MaxMana
              | Speed
              | Defence
              | Attack
    deriving(Show, Eq, Generic)
instance Serialize StatName

-- |Default stat values
basicStats = Stats {
        stHealth = 0,
        stMaxHealth = 1,
        stMana = 0,
        stMaxMana = 1,
        stSpeed = 0,
        stDefence = 0,
        stAttack = 0
    }
   
-- |Structure of game object fields   
data GameObject = GameObject {
        goId :: ILKey -- ^Id number of the object
       ,goType :: ObjectType -- ^Type identifier of the object
       ,goStats :: Maybe Stats -- ^Optional stats structure for the object
       ,goTarget :: ILKey -- ^The object that this object is targeting
       ,goTargetDirection :: Vector2D -- ^The direction this object is targeting
       ,goTeam :: GameTeam -- ^The team this object is a member of
       ,goClass :: PlayerClass -- ^The class of this object
       ,goLane :: MapLane -- ^The lane this object belongs to
       ,goPos :: Vector2D -- ^The current position of this object
       ,goSize :: Vector2D -- ^The size of this object
       ,goIsDead :: Bool -- ^Wether or not this object is dead
    }
    deriving(Show, Generic)
instance Serialize GameObject

-- |Computes equality between two game objects by comparing their ids
instance Eq GameObject where
    a == b = (goId a) == (goId b)
    a /= b = (goId a) /= (goId b)
   
-- |Default game object values
basicGameObject = GameObject {
        goId = (-1),
        goType = NotAnObject,
        goStats = Nothing,
        goTarget = (-1),
        goTargetDirection = zeroVector,
        goTeam = Neutral,
        goClass = Mage,
        goLane = Middle,
        goPos = zeroVector,
        goSize = zeroVector,
        goIsDead = False
    }