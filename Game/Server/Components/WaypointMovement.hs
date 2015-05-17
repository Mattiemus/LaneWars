{-# LANGUAGE Arrows #-}

module Game.Server.Components.WaypointMovement where

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

-----------------------
-- Waypoint movement --
-----------------------

-- |Input structure for the waypoint movement component
data WaypointMovementInput = WaypointMovementInput {
        wmiSpeed :: Double -- ^The speed at which the object should move at
       ,wmiCurrPos :: Vector2D -- ^The current position of the object
       ,wmiAllObjects :: [GameObject] -- ^The list of all other objects in the scene
       ,wmiEnemyTeam :: GameTeam -- ^The enemy game team
       ,wmiFrontOfPack :: Bool -- ^True if the the object is the front of the pack
    }

-- |Output structure for the waypoint movement component
data WaypointMovementOutput = WaypointMovementOutput {
        wmoAtFinalWaypointEvent :: Yampa.Event () -- ^Event fired when the object reaches the final waypoint
       ,wmoMoveDelta :: Vector2D -- ^Amount by which the object should move
    }

-- |Component that handles waypoint movement. Takes in a list of
-- positions and moves the object along them at the specified speed, 
-- firing an even when the final waypoint is reached. The object
-- will stop when near an obstacle
waypointMovement :: [Vector2D] -- ^List of waypoint positions
                 -> SF WaypointMovementInput WaypointMovementOutput
waypointMovement waypoints = proc input -> do
    -- Movement
    let position = wmiCurrPos input
    rec waypointIndex <- dAccumHoldBy (+) 0 -< atWaypointEvent `tag` 1
        let atFinalWaypoint = waypointIndex >= length waypoints 
            atFinalWaypointEvent = condEvent atFinalWaypoint
            atWaypointEvent = condEvent (vectorDistance position targetPos < 3)
            targetPos = if not atFinalWaypoint
                then waypoints !! waypointIndex
                else zeroVector
            moveVector = targetPos ^-^ position
            moveDelta = if vectorDistance zeroVector moveVector < 3
                then zeroVector
                else (wmiSpeed input) *^ (normalize moveVector)
    -- Look around for obstructions
    let nearbyEnemyUnits = findWithinRangeOfPoint position 150 (filterObjByTeam (wmiEnemyTeam input) (filterObjByType [Turret, Minion, Nexus] (wmiAllObjects input)))
        nearbyAliveEnemyUnits = filter (\obj -> not (goIsDead obj)) nearbyEnemyUnits
        canMove = null nearbyAliveEnemyUnits
    -- Return output
    returnA -< WaypointMovementOutput {
            wmoAtFinalWaypointEvent = atFinalWaypointEvent,
            wmoMoveDelta = if canMove 
                then moveDelta 
                else zeroVector
        }