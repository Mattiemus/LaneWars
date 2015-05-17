{-# LANGUAGE Arrows #-}

module Game.Server.Objects.Network where

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

-- |Game object that manages new connecting clients; placing them in alternating
-- teams
netManager :: Object
netManager = proc objInput -> do
    newClients <- clientsConnected -< oiNetwork objInput
    rec newClientTeam <- dHold Red -< newClients `tag` (enemyTeam newClientTeam)
    returnA -< (defaultObjOutput objInput) {
            ooSpawnRequests = newClients `tagUsing` map (clientConnection newClientTeam)
        }

-- |Game object that manages a connected client
clientConnection :: GameTeam -- ^Team the client belongs too
                 -> Socket -- ^Socket the client connected on
                 -> Object
clientConnection team sock = proc objInput -> do
    -- Attack requests
    attackRequest <- attackRequest sock -< oiNetwork objInput
    let attackMana = 1
    -- Components
    basicComponent <- basicOwnedObject sock -< BasicOwnedObjectInput {
            booiNetwork = oiNetwork objInput
        }
    damageComponent <- projectileDamage (enemyTeam team) -< ProjectileDamageInput {
            pdiObjId = oiId objInput,
            pdiAllCollisions = oiCollidingWith objInput
        }
    healthRegen <- repeatedly 1 2 -< ()
    manaRegen <- repeatedly 1 2 -< ()
    rec statsComponent <- objectStats startStats -< ObjectStatsInput {
                osiHealthChanged = mergeBy (+) (mergeBy (+) (pdoDamageEvent damageComponent) healthRegen)
                                               ((osoDeathEvent statsComponent) `tag` (stMaxHealth (osoStats statsComponent))),
                osiMaxHealthChanged = noEvent,
                osiManaChanged = mergeBy (+) (mergeBy (+) ((osoDeathEvent statsComponent) `tag` (stMaxMana (osoStats statsComponent))) manaRegen)
                                             (if (stMana (osoStats statsComponent)) >= attackMana then attackRequest `tag` (-attackMana) else noEvent),
                osiMaxManaChanged = noEvent,
                osiSpeedChanged = noEvent,
                osiDefenceChanged = noEvent,
                osiAttackChanged = noEvent
            }
    let stats = osoStats statsComponent
    -- Announce the players details
    announceEvent <- delayEvent 0.25 <<< now () -< ()
    -- Object representation
    position <- hold spawnPosition -< boooPositionChanged basicComponent
    frameEvent <- onFrameEvent -< ()
    let obj = (defaultGameObject objInput) {
            goType = Player,
            goPos = position,
            goSize = vector2 32 32,
            goStats = Just stats,
            goTeam = team
        } 
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooKillRequest = boooOwnerDisconnected basicComponent,
            ooSpawnRequests = catEvents [if (stMana (osoStats statsComponent)) >= attackMana then attackRequest `tagUsing` (playerAttackProjectile position team) else noEvent],
            ooGlobalMessages = catEvents [boooOwnerDisconnected basicComponent `tag` Destroy (goId obj),
                                          osoDeathEvent statsComponent `tag` Kill (goId obj),
                                          osoHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) Health),
                                          osoMaxHealthChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxHealth),
                                          osoManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) Mana),
                                          osoMaxManaChanged statsComponent `tagUsing` (UpdateStats (goId obj) MaxMana),
                                          osoSpeedChanged statsComponent `tagUsing` (UpdateStats (goId obj) Speed),
                                          osoDefenceChanged statsComponent `tagUsing` (UpdateStats (goId obj) Defence),
                                          osoAttackChanged statsComponent `tagUsing` (UpdateStats (goId obj) Attack)],
            ooGlobalExceptMessages = catEvents [boooGlobalAnnounceEvent basicComponent `tag` (sock, Create obj),
                                                frameEvent `tag` (sock, Move (goId obj) position)],
            ooMessages = foldl (mergeBy (++)) noEvent [boooSocketAnnounceEvent basicComponent `tagUsing` (map (\s -> (s, Create obj))),
                                                       announceEvent `tag` [(sock, PlayerData (goId obj) (goTeam obj) (goClass obj))]],
            ooGameObject = obj
        }
    where
        spawnPosition | team == Red = vector2 100 2900
                      | team == Blue  = vector2 2900 100
        startStats = Stats {
                    stHealth = 350,
                    stMaxHealth = 350,
                    stMana = 375,
                    stMaxMana = 375,
                    stSpeed = 300,
                    stDefence = 0,
                    stAttack = 0
                }
                
-- |Game object for a player-spawned projectile
playerAttackProjectile :: Vector2D -- ^The initial position of the projectile
                       -> GameTeam -- ^The team the projectile belongs too
                       -> (Int, Vector2D) -- ^The attack number and direction the projectile should move along
                       -> Object
playerAttackProjectile pos team (attackId, direction) = proc objInput -> do
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
    rec directionalProjectileComponent <- directionalProjectile (enemyTeam team) 5 direction -< DirectionalProjectileInput {
            dpiAllCollisions = oiCollidingWith objInput,
            dpiAllObjects = oiAllObjects objInput,
            dpiSpeed = fromIntegral (stSpeed stats),
            dpiCurrPos = position
        }
        position <- (^+^ pos) ^<< integral -< dpoMoveDelta directionalProjectileComponent
    -- Basic object representation
    let obj = (defaultGameObject objInput) {
            goType = PlayerProjectile attackId,
            goTeam = team,
            goTargetDirection = direction,
            goPos = position,
            goStats = Just stats,
            goSize = vector2 16 16
        }  
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooKillRequest = dpoHitTargetEvent directionalProjectileComponent,
            ooGlobalMessages = catEvents [dpoHitTargetEvent directionalProjectileComponent `tag` Destroy (goId obj),
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
                    stSpeed = if attackId == 1 then 300 else 125,
                    stDefence = 0,
                    stAttack = if attackId == 1 then 50 else 150
                }
    