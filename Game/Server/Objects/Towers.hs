{-# LANGUAGE Arrows #-}

module Game.Server.Objects.Towers where

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
import Game.Server.Components.BasicComponents
import Game.Server.Networking

-- |Game object for a turret. Fires projectiles and enemy minions and players
turretObject :: Vector2D -- ^Position of the turret
             -> MapLane -- ^Lane the turret belongs to
             -> GameTeam -- ^Team the turret belongs too
             -> Object
turretObject pos lane team = proc objInput -> do
    -- Components
    basicComponent <- basicObject -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    delayedSockAnnounce <- delayEvent  (1 / 5) -< booSocketAnnounceEvent basicComponent
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
    isDead <- hold False -< osoDeathEvent statsComponent `tag` True
    deathEvent <- edge -< isDead
    -- Object representation
    let obj = (defaultGameObject objInput) {
            goType = Turret,
            goTeam = team,
            goLane = lane,
            goPos = pos,
            goStats = Just stats,
            goSize = vector2 95 95,
            goIsDead = isDead
        } 
    -- Components
    projectileSourceComponent <- projectileSource 350 1.0 [enemyTeam team] [Minion, Player] -< ProjectileSourceInput {
            psiAllObjects = oiAllObjects objInput,
            psiSourceObject = obj
        }
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooSpawnRequests = gate (catEvents [psiFireEvent projectileSourceComponent `tagUsing` (turretProjectile projectileOrigin team)]) (not isDead),
            ooGlobalMessages = catEvents [booGlobalAnnounceEvent basicComponent `tag` Create obj,
                                          deathEvent `tag` Kill (goId obj),
                                          osoHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) Health),
                                          osoMaxHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxHealth),
                                          osoManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) Mana),
                                          osoMaxManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxMana),
                                          osoSpeedChanged statsComponent `tagUsing` (UpdateStats (goId obj) Speed),
                                          osoDefenceChanged statsComponent `tagUsing` (UpdateStats (goId obj) Defence),
                                          osoAttackChanged statsComponent `tagUsing` (UpdateStats (goId obj) Attack)],
            ooMessages = mergeBy (++) (booSocketAnnounceEvent basicComponent `tagUsing` (map (\s -> (s, Create obj))))
                                      (gate (delayedSockAnnounce `tagUsing` (map (\s -> (s, Kill (goId obj))))) isDead),
            ooGameObject = obj
        }   
    where
        projectileOrigin = pos ^+^ vector2 39.5 39.5
        startStats = Stats {
                    stHealth = 1800,
                    stMaxHealth = 1800,
                    stMana = 0,
                    stMaxMana = 0,
                    stSpeed = 0,
                    stDefence = 0,
                    stAttack = 0
                }
        
-- |Projectile fired from a turret
turretProjectile :: Vector2D -- ^Initial position of the projectile
                 -> GameTeam -- ^Team the projectile belongs too
                 -> ILKey -- ^ID of the object this projectile is targeting
                 -> Object
turretProjectile pos team target = proc objInput -> do
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
            goType = TurretProjectile,
            goTeam = team,
            goTarget = target,
            goPos = position,
            goStats = Just stats,
            goSize = vector2 16 16
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
                    stSpeed = 300,
                    stDefence = 0,
                    stAttack = 75
                }
    