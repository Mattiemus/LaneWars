{-# LANGUAGE Arrows #-}

module Game.Server.Objects.Minions where

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
import Game.Server.Components.Projectiles
import Game.Server.Components.WaypointMovement
import Game.Server.Components.BasicComponents
import Game.Server.Networking

-- |Server-side game object for a minion
minionObject :: Vector2D -- ^Start position of the minion
             -> MapLane -- ^The lane the minion should follow
             -> GameTeam -- ^The team the minion belongs too
             -> Bool -- ^True if the minion is at the front of the line of minions
             -> Object
minionObject pos lane team frontOfPack = proc objInput -> do
    -- Components
    basicComponent <- basicObject -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    damageComponent <- projectileDamage (enemyTeam team) -< ProjectileDamageInput {
            pdiObjId = oiId objInput,
            pdiAllCollisions = oiCollidingWith objInput
        }
    statsComponent <- objectStats startStats -< ObjectStatsInput {
            osiHealthChanged = pdoDamageEvent damageComponent,
            osiMaxHealthChanged = noEvent,
            osiManaChanged = noEvent,
            osiMaxManaChanged = noEvent,
            osiSpeedChanged = noEvent,
            osiDefenceChanged = noEvent,
            osiAttackChanged = noEvent
        }
    let stats = osoStats statsComponent
    rec waypointMovementComponent <- waypointMovement (minionWaypoints lane team) -< WaypointMovementInput {
            wmiSpeed = fromIntegral (stSpeed stats),
            wmiCurrPos = position,
            wmiAllObjects = oiAllObjects objInput,
            wmiEnemyTeam = enemyTeam team,
            wmiFrontOfPack = frontOfPack
        }
        position <- (^+^ pos) ^<< integral -< wmoMoveDelta waypointMovementComponent
    -- Object representation
    let obj = (defaultGameObject objInput) {
            goType = Minion,
            goTeam = team,
            goLane = lane,
            goPos = position,
            goStats = Just stats,
            goSize = vector2 24 24            
        }
    -- Components
    projectileSourceComponent <- projectileSource 150 1.5 [enemyTeam team] [Minion, Turret, Nexus, Player] -< ProjectileSourceInput {
            psiAllObjects = oiAllObjects objInput,
            psiSourceObject = obj
        }
    -- Movement
    frameEvent <- onFrameEvent -< ()
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooKillRequest = lMerge (wmoAtFinalWaypointEvent waypointMovementComponent) (osoDeathEvent statsComponent),
            ooSpawnRequests = catEvents [psiFireEvent projectileSourceComponent `tagUsing` (minionProjectile (position ^+^ vector2 8 8) team)],
            ooGlobalMessages = catEvents [wmoAtFinalWaypointEvent waypointMovementComponent `tag` Destroy (goId obj),
                                          osoDeathEvent statsComponent `tag` Kill (goId obj),
                                          booGlobalAnnounceEvent basicComponent `tag` Create obj,
                                          frameEvent `tag` Move (goId obj) position,
                                          osoHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) Health),
                                          osoMaxHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxHealth),
                                          osoManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) Mana),
                                          osoMaxManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxMana),
                                          osoSpeedChanged statsComponent `tagUsing` (UpdateStats (goId obj) Speed),
                                          osoDefenceChanged statsComponent `tagUsing` (UpdateStats (goId obj) Defence),
                                          osoAttackChanged statsComponent `tagUsing` (UpdateStats (goId obj) Attack)],
            ooMessages = booSocketAnnounceEvent basicComponent `tagUsing` (map (\s -> (s, Create obj))),
            ooGameObject = obj
        }  
    where
        startStats = Stats {
                    stHealth = 500,
                    stMaxHealth = 500,
                    stMana = 0,
                    stMaxMana = 0,
                    stSpeed = 150,
                    stDefence = 0,
                    stAttack = 0
                }
        minionWaypoints Top Blue = [vector2 2217.5 122.5, vector2 128 128, vector2 122.5 2217.5, vector2 314 2764]
        minionWaypoints Middle Blue = [vector2 2447.5 597.5, vector2 314 2764]
        minionWaypoints Bottom Blue = [vector2 2947.5 847.5, vector2 2944 2944, vector2 887.5 2947.5, vector2 314 2764]
        minionWaypoints Top Red = [vector2 122.5 2217.5, vector2 128 128, vector2 2177.5 122.5, vector2 2764 314]
        minionWaypoints Middle Red = [vector2 597.5 2447.5, vector2 2764 314]
        minionWaypoints Bottom Red = [vector2 887.5 2947.5, vector2 2944 2944, vector2 2947.5 847.5, vector2 2764 314]

-- |Game object of a projectile fired by a minion
minionProjectile :: Vector2D -- ^Start position of the projectile
                 -> GameTeam -- ^Team the projectile belongs too
                 -> ILKey -- ^ID of the object this projectile is targeting
                 -> Object
minionProjectile pos team target = proc objInput -> do
    -- Components
    basicComponent <- basicObject -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    statsComponent <- objectStats startStats -< ObjectStatsInput {
            osiHealthChanged = noEvent,
            osiMaxHealthChanged = noEvent,
            osiManaChanged = noEvent,
            osiMaxManaChanged = noEvent,
            osiSpeedChanged = noEvent,
            osiDefenceChanged = noEvent,
            osiAttackChanged = noEvent
        }
    let stats = osoStats statsComponent
    rec trackingProjectileComponent <- trackingProjectile 1 target -< TrackingProjectileInput {
            tpiAllCollisions = oiCollidingWith objInput,
            tpiAllObjects = oiAllObjects objInput,
            tpiSpeed = fromIntegral (stSpeed stats),
            tpiCurrPos = position
        }
        position <- (^+^ pos) ^<< integral -< tpoMoveDelta trackingProjectileComponent
    -- Basic object representation
    let obj = (defaultGameObject objInput) {
            goType = MinionProjectile,
            goTeam = team,
            goTarget = target,
            goPos = position,
            goStats = Just stats,
            goSize = vector2 8 8
        }  
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooKillRequest = tpoHitTargetEvent trackingProjectileComponent,
            ooGlobalMessages = catEvents [tpoHitTargetEvent trackingProjectileComponent `tag` Destroy (goId obj),
                                          booGlobalAnnounceEvent basicComponent `tag` Create obj,
                                          osoHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) Health),
                                          osoMaxHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxHealth),
                                          osoManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) Mana),
                                          osoMaxManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxMana),
                                          osoSpeedChanged statsComponent `tagUsing` (UpdateStats (goId obj) Speed),
                                          osoDefenceChanged statsComponent `tagUsing` (UpdateStats (goId obj) Defence),
                                          osoAttackChanged statsComponent `tagUsing` (UpdateStats (goId obj) Attack)],
            ooMessages = booSocketAnnounceEvent basicComponent `tagUsing` (map (\s -> (s, Create obj))),
            ooGameObject = obj
    }
    where
        startStats = Stats {
                    stHealth = 1,
                    stMaxHealth = 1,
                    stMana = 0,
                    stMaxMana = 0,
                    stSpeed = 380,
                    stDefence = 0,
                    stAttack = 15
                }