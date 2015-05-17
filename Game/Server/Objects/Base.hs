{-# LANGUAGE Arrows #-}

module Game.Server.Objects.Base where

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
import Game.Server.Objects.Minions
import Game.Server.Components.Projectiles
import Game.Server.Components.BasicComponents
import Game.Server.Networking

-- |Game object representing a player base
nexusObject :: Vector2D -- ^Position of the base
            -> GameTeam -- ^Game team of the base
            -> Object
nexusObject pos team = proc objInput -> do
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
    firstDeath <- once -< osoDeathEvent statsComponent
    -- Spawn minions
    rec createMinionsEvent <- repeatedly 30 5 -< ()
        spawnTimer <- repeatedly 1 (-1) -< ()
        minionsToSpawn <- dAccumHoldBy (+) 0 -< lMerge createMinionsEvent spawnMinionEvent
        let spawnMinionEvent = gate spawnTimer (minionsToSpawn > 0)
            topMinionPos = pos ^-^ (vector2 22 22)
            midMinionPos = if team == Red
                then pos ^+^ (vector2 130 (-16))
                else pos ^+^ (vector2 (-16) 130)
            botMinionPos = pos ^+^ (vector2 138 138) 
    -- Object representation
    let obj = (defaultGameObject objInput) {
            goType = Nexus,
            goTeam = team,
            goPos = pos,
            goStats = Just stats,
            goSize = vector2 128 128
        } 
    -- Return object state
    returnA -< (defaultObjOutput objInput) {
            ooSpawnRequests = catEvents [spawnMinionEvent `tag` minionObject topMinionPos Top team (minionsToSpawn == 5),
                                         spawnMinionEvent `tag` minionObject midMinionPos Middle team (minionsToSpawn == 5),
                                         spawnMinionEvent `tag` minionObject botMinionPos Bottom team (minionsToSpawn == 5)],
            ooGlobalMessages = catEvents [booGlobalAnnounceEvent basicComponent `tag` Create obj,
                                          firstDeath `tag` TeamWon (enemyTeam team),
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
                    stHealth = 2000,
                    stMaxHealth = 2000,
                    stMana = 0,
                    stMaxMana = 0,
                    stSpeed = 0,
                    stDefence = 0,
                    stAttack = 0
                }