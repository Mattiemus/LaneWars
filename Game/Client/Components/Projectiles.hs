{-# LANGUAGE Arrows #-}

module Game.Client.Components.Projectiles where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Data.Maybe

import Numeric.IEEE

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.Object
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-------------------------
-- Tracking projectile --
-------------------------

-- |Input structure for the tracking projectile component
data TrackingProjectileInput = TrackingProjectileInput {
        tpiAllCollisions :: [GameObject] -- ^List of objects colliding with the projectile
       ,tpiAllObjects :: [GameObject] -- ^All game objects in the scene
       ,tpiSpeed :: Double -- ^The speed the projectile should travel at
       ,tpiCurrPos :: Vector2D -- ^The current position of the projectile
    }

-- |Output structure for the tracking projectile component
data TrackingProjectileOutput = TrackingProjectileOutput {
        tpoHitTargetEvent :: Yampa.Event () -- ^Event fired when the projectile has hit its target
       ,tpoMoveDelta :: Vector2D -- ^Amount to move by
    }

-- |Component for a projectile that should track and chase a specific object
trackingProjectile :: GameObject -- ^The projectile object
                   -> Time -- ^The maximum lifetime of the projectile
                   -> SF TrackingProjectileInput TrackingProjectileOutput
trackingProjectile obj maxLifeTime = proc input -> do
    -- Move
    let isColliding = isJust (findObjById (goTarget obj) (tpiAllCollisions input))
        objToTrack = findObjById (goTarget obj) (tpiAllObjects input)
        hasTarget = isJust objToTrack
        targetObj = maybe basicGameObject (id) objToTrack
        targetPos = middleOfObject targetObj
        moveDelta = (tpiSpeed input) *^ (normalize (targetPos ^-^ (tpiCurrPos input)))
    -- Handle collisions
    collisionEvent <- edge -< isColliding
    targetLostEvent <- edge -< not hasTarget
    maxLifeTimeReachedEvent <- delayEvent maxLifeTime <<< now () -< ()
    let hitTargetEvent = foldl (lMerge) collisionEvent [targetLostEvent, maxLifeTimeReachedEvent]
    -- Return object state
    returnA -< TrackingProjectileOutput {
            tpoHitTargetEvent = hitTargetEvent,
            tpoMoveDelta = moveDelta
        }    
        
-------------------------
-- Directional projectile --
-------------------------

-- |Input structure for the directional projectile 
data DirectionalProjectileInput = DirectionalProjectileInput {
        dpiAllCollisions :: [GameObject] -- ^A list of objects colliding with the projectile
       ,dpiAllObjects :: [GameObject] -- ^List of all objects within the scene
       ,dpiSpeed :: Double -- ^The speed the projectile should travel at
       ,dpiCurrPos :: Vector2D -- ^Current position of the projectile
    }

-- |Output structure for the directional projectile
data DirectionalProjectileOutput = DirectionalProjectileOutput {
        dpoHitTargetEvent :: Yampa.Event () -- ^Event fired when the projectile hits an object
       ,dpoMoveDelta :: Vector2D -- ^Amount to move by
    }

-- |A projectile that moves in a fixed direction
directionalProjectile :: GameTeam -- ^The enemy team
                      -> GameObject  -- ^The projectile object
                      -> Time -- ^Maximum lifetime of the projectile
                      -> SF DirectionalProjectileInput DirectionalProjectileOutput
directionalProjectile enemyTeam obj maxLifeTime = proc input -> do
    -- Move
    let isColliding = isJust (findObjByTeam enemyTeam (filter (\obj -> not (goIsDead obj)) (dpiAllCollisions input)))
        moveDelta = (dpiSpeed input) *^ (normalize (goTargetDirection obj))
    -- Handle collisions
    collisionEvent <- edge -< isColliding
    maxLifeTimeReachedEvent <- delayEvent maxLifeTime <<< now () -< ()
    let hitTargetEvent = lMerge collisionEvent maxLifeTimeReachedEvent
    -- Return object state
    returnA -< DirectionalProjectileOutput {
            dpoHitTargetEvent = hitTargetEvent,
            dpoMoveDelta = moveDelta
        }    