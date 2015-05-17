{-# LANGUAGE Arrows #-}

module Game.Client.Objects.Network where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.Objects.Towers
import Game.Client.Objects.Input
import Game.Client.Objects.Minions
import Game.Client.Objects.Base
import Game.Client.Components.BasicComponents
import Game.Client.Components.Projectiles
import Game.Client.Object
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-- |Manager that handles object creation events, generating
-- the game object and submitting a spawn request for it
netManager :: Object
netManager = proc objInput -> do
    -- Connection events
    connSuccEvent <- connectionSuccess -< oiNetwork objInput
    connFailedEvent <- connectionFailed -< oiNetwork objInput
    -- Object creation
    newPlayerObjectEvent <- createObject Player -< oiNetwork objInput
    newPlayerProjectile1ObjectEvent <- createObject (PlayerProjectile 1) -< oiNetwork objInput
    newPlayerProjectile2ObjectEvent <- createObject (PlayerProjectile 2) -< oiNetwork objInput
    newTurretObjectEvent <- createObject Turret -< oiNetwork objInput
    newTurretProjectileObjectEvent <- createObject TurretProjectile -< oiNetwork objInput
    newMinionObjectEvent <- createObject Minion -< oiNetwork objInput
    newMinionProjectileObjectEvent <- createObject MinionProjectile -< oiNetwork objInput
    newNexusObjectEvent <- createObject Nexus -< oiNetwork objInput
    -- Return state
    returnA -< (defaultObjOutput objInput) { 
            ooSpawnRequests = foldl (mergeBy (++)) noEvent [connSuccEvent `tag` [playerObject],
                                                            newPlayerObjectEvent `tagUsing` map networkPlayer,
                                                            newPlayerProjectile1ObjectEvent `tagUsing` map networkPlayerProjectile,
                                                            newPlayerProjectile2ObjectEvent `tagUsing` map networkPlayerProjectile,
                                                            newTurretObjectEvent `tagUsing` map turretObject,
                                                            newTurretProjectileObjectEvent `tagUsing` map towerProjectile,
                                                            newMinionObjectEvent `tagUsing` map minionObject,
                                                            newMinionProjectileObjectEvent `tagUsing` map minionProjectile,
                                                            newNexusObjectEvent `tagUsing` map nexusObject]
        }
  
-- |Game object that draws the map background
mapBackground :: Object
mapBackground = proc objInput -> do
     -- Return state
    returnA -< (defaultObjOutput objInput) {
        ooGraphic = draw backgroundImage (Mask Nothing 0 0),
        ooGraphicLayer = GameLayer 0,
        ooGameObject = (defaultGameObject objInput) {
                goPos = zeroVector,
                goSize = vector2 3072 3072
            }
    }      
    
-- |Game object for a player not managed by this client
networkPlayer :: GameObject -- ^The representation of this player that constructed this game object
              -> Object
networkPlayer obj = proc objInput -> do
    -- Components
    basicComponent <- basicObject obj -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    statsComponent <- objectStats obj -< ObjectStatsInput {
            osiNetwork = oiNetwork objInput
        }
    let stats = osoStats statsComponent
        position = booPosition basicComponent
    -- Components    
    healthbarComponent <- healthbarDisplay 1.5 (vector2 (-6) (-12)) 44 6 -< HealthbarDisplayInput {
            hdiHealthChangedEvent = osoHealthChanged statsComponent,
            hdiObjectPosition = position,
            hdiCurrentHealth = stHealth stats,
            hdiCurrentMaxHealth = stMaxHealth stats
        }
    -- Return state
    let (x, y) = vectorRoundedComponents position
        team = goTeam obj
    returnA -< (defaultObjOutput objInput) { 
        ooKillRequest = booObjectDestroyed basicComponent,
        ooGraphic = drawAll [draw (playerImage team) (Mask Nothing x y),
                             hdoHealthbarGraphic healthbarComponent],
        ooGraphicLayer = GameLayer 10,
        ooGameObject = obj {
                goPos = position,
                goStats = Just stats
            }
    }
    
-- |Projectile fired by a networked player
networkPlayerProjectile :: GameObject -- ^Game object representation at creation time of this object
                        -> Object
networkPlayerProjectile obj = proc objInput -> do
    -- Components
    basicComponent <- basicObject obj -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    statsComponent <- objectStats obj -< ObjectStatsInput {
            osiNetwork = oiNetwork objInput
        }
    let stats = osoStats statsComponent
    rec projectileComponent <- directionalProjectile (enemyTeam (goTeam obj)) obj 5 -< DirectionalProjectileInput {
                dpiAllCollisions = oiCollidingWith objInput,
                dpiAllObjects = oiAllObjects objInput,
                dpiSpeed = fromIntegral (stSpeed stats),
                dpiCurrPos = position
            }
        position <- (^+^ (goPos obj)) ^<< integral -< dpoMoveDelta projectileComponent
    -- Return state
    let (x, y) = vectorRoundedComponents position
    returnA -< (defaultObjOutput objInput) { 
        ooKillRequest = lMerge (booObjectDestroyed basicComponent) (dpoHitTargetEvent projectileComponent),
        ooGraphic = draw turretProjectileImage (Mask Nothing x y),
        ooGraphicLayer = GameLayer 6,
        ooGameObject = obj {
                goPos = position
            }
    }
    