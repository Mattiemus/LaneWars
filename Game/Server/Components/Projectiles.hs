{-# LANGUAGE Arrows #-}

module Game.Server.Components.Projectiles where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Safe

import IdentityList

import Network.Socket

import Data.Maybe

import Numeric.IEEE

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Arrows
import Game.Shared.Object
import Game.Shared.Physics
import Game.Server.Object
import Game.Server.Networking

-------------------------
-- Tracking projectile --
-------------------------

-- |Input for the tracking projectile component
data TrackingProjectileInput = TrackingProjectileInput {
        tpiAllCollisions :: [GameObject] -- ^List of all objects colliding with the projectile
       ,tpiAllObjects :: [GameObject] -- ^List of all other objects in the scene
       ,tpiSpeed :: Double -- ^Speed at which the projectile should travel
       ,tpiCurrPos :: Vector2D -- ^Current position of the projectile
    }

-- |Output for the tracking projectile component
data TrackingProjectileOutput = TrackingProjectileOutput {
        tpoHitTargetEvent :: Yampa.Event () -- ^Event that fires when the target object is hit
       ,tpoMoveDelta :: Vector2D -- ^Amount to move by
    }

-- |Component that moves an object to follow another game object
trackingProjectile :: Time -- ^Maximum lifetime of the projectile
                   -> ILKey -- ^ID of the object this projectile is targeting
                   -> SF TrackingProjectileInput TrackingProjectileOutput
trackingProjectile maxLifeTime target = proc input -> do
    -- Move
    let isColliding = isJust (findObjById target (tpiAllCollisions input))
        objToTrack = findObjById target (tpiAllObjects input)
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

-- |Input structure for a directional projectile
data DirectionalProjectileInput = DirectionalProjectileInput {
        dpiAllCollisions :: [GameObject] -- ^List of objects this projectile is colliding with
       ,dpiAllObjects :: [GameObject] -- ^List of all other objects in the scene
       ,dpiSpeed :: Double -- ^Speed at which this projectile should travel
       ,dpiCurrPos :: Vector2D -- ^Current position of the projectile
    }

-- |Output structure for a directional projectile
data DirectionalProjectileOutput = DirectionalProjectileOutput {
        dpoHitTargetEvent :: Yampa.Event () -- ^Event that fires when a target is hit
       ,dpoMoveDelta :: Vector2D -- ^Amount to move by
    }

-- |Component for a project that moves in a fixed path
directionalProjectile :: GameTeam -- ^Team this projectile belongs to
                      -> Time -- ^Maximum lifetime of the projectile
                      -> Vector2D -- ^Direction the projectile should move along
                      -> SF DirectionalProjectileInput DirectionalProjectileOutput
directionalProjectile enemyTeam maxLifeTime direction = proc input -> do
    -- Move
    let isColliding = isJust (findObjByTeam enemyTeam (filter (\obj -> not (goIsDead obj)) (dpiAllCollisions input)))
        moveDelta = (dpiSpeed input) *^ (normalize direction)
    -- Handle collisions
    collisionEvent <- edge -< isColliding
    maxLifeTimeReachedEvent <- delayEvent maxLifeTime <<< now () -< ()
    let hitTargetEvent = lMerge collisionEvent maxLifeTimeReachedEvent
    -- Return object state
    returnA -< DirectionalProjectileOutput {
            dpoHitTargetEvent = hitTargetEvent,
            dpoMoveDelta = moveDelta
        }    
  
-----------------------
-- Projectile source --
-----------------------      

-- |Input structure for a projectile source
data ProjectileSourceInput = ProjectileSourceInput {
        psiAllObjects :: [GameObject] -- ^List of all objects in the scene
       ,psiSourceObject :: GameObject -- ^The object to act as the projectile source
    }

-- |Output structure for a projectile source
data ProjectileSourceOutput = ProjectileSourceOutput {
        psiFireEvent :: Yampa.Event ILKey -- ^Event fired when a projectile should be emitted, with the target object id
    }
        
-- |Component for a projectile source. Fires projectiles at enemy passing minions and players
projectileSource :: Double -- ^Maximum range to fire
                 -> Time -- ^Amount of time between firings
                 -> [GameTeam] -- ^List of teams to target
                 -> [ObjectType] -- ^List of object types to target
                 -> SF ProjectileSourceInput ProjectileSourceOutput
projectileSource range fireRate targetTeams targetTypes = proc input -> do
    -- Fire projectiles at the nearest object until it dies or leaves turret range
    fireTimerEvent <- repeatedly fireRate () -< ()  
    let nearestObjects = filter (\obj -> not (goIsDead obj)) (findWithinRangeOf (psiSourceObject input) range (filterValidTargets (psiAllObjects input)))
        mayNearestObject = headMay nearestObjects
        mayNearestObjectId = maybe invalidTarget (goId) mayNearestObject
    rec currentTarget <- hold invalidTarget -< retargetEvent `tag` mayNearestObjectId
        targetExists <- arr isJust -< if currentTarget == invalidTarget 
            then Nothing
            else findObjById currentTarget nearestObjects
        retargetEvent <- delayEvent epsilon -< condEvent (not targetExists)
    let projectileEvent = gate fireTimerEvent targetExists
    -- Return object state
    returnA -< ProjectileSourceOutput {
            psiFireEvent = projectileEvent `tag` currentTarget
        }
    where
        invalidTarget = (-1)
        filterValidTargets = filter (\o -> (isTargetableType o) && (isTargetableTeam o))    
        isTargetableType obj = goType obj `elem` targetTypes
        isTargetableTeam obj = goTeam obj `elem` targetTeams
        
-------------------
-- Damage on hit --
------------------- 

-- |Input structure for projectile damage component
data ProjectileDamageInput = ProjectileDamageInput {
        pdiObjId :: ILKey -- ^Id of the object
       ,pdiAllCollisions :: [GameObject] -- ^List of all objects colliding with the damageable object
    }

-- |Output structure for projectile damage component
data ProjectileDamageOutput = ProjectileDamageOutput {
        pdoDamageEvent :: Yampa.Event Int -- ^Event fired when the object should be damaged, with the damage amount
    }
    
-- |Component that handles projectile damage. Fires an event which stores how much
-- health the object should be damaged by each time the object is hit
projectileDamage :: GameTeam -- ^The enemy team
                 -> SF ProjectileDamageInput ProjectileDamageOutput
projectileDamage enemyTeam = proc input -> do
    -- Look for any collisions
    let projectileCollisions = filterObjByType [TurretProjectile, MinionProjectile, PlayerProjectile 1, PlayerProjectile 2] (pdiAllCollisions input)
        validProjectileCollisions = filter (\obj -> (goTarget obj) == (pdiObjId input) || (goTargetDirection obj) /= zeroVector) projectileCollisions
        enemyValidProjectileCollisions = filter (\obj -> (goTeam obj) == enemyTeam) validProjectileCollisions
        projectileDamages = map (stAttack . fromJust . goStats) enemyValidProjectileCollisions
        damageSum = sum projectileDamages
    -- Return a potential event
    returnA -< ProjectileDamageOutput {
            pdoDamageEvent = taggedCondEvent (damageSum > 0) (-damageSum)
        }