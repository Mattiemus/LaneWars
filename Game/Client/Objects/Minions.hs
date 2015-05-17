{-# LANGUAGE Arrows #-}

module Game.Client.Objects.Minions where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.Object
import Game.Client.Components.BasicComponents
import Game.Client.Components.Projectiles
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-- |Game object for a a minion
minionObject :: GameObject -- ^Object representation sent by server
             -> Object
minionObject obj = proc objInput -> do
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
    healthbarComponent <- healthbarDisplay 1.5 (vector2 (-4) (-12)) 32 6 -< HealthbarDisplayInput {
            hdiHealthChangedEvent = osoHealthChanged statsComponent,
            hdiObjectPosition = position,
            hdiCurrentHealth = stHealth stats,
            hdiCurrentMaxHealth = stMaxHealth stats
        }
    -- Return state
    let (x, y) = vectorRoundedComponents position
        team = goTeam obj
    returnA -< (defaultObjOutput objInput) { 
        ooKillRequest = lMerge (booObjectDestroyed basicComponent) (osoDeathEvent statsComponent),
        ooGraphic = drawAll [draw (minionImage team) (Mask Nothing x y),
                             hdoHealthbarGraphic healthbarComponent],
        ooGraphicLayer = GameLayer 5,
        ooGameObject = obj {
                goPos = position,
                goStats = Just stats
            }
    }
    
-- |Projectile object fired from a minion
minionProjectile :: GameObject -- ^Object representation sent by server
                 -> Object
minionProjectile obj = proc objInput -> do
    -- Components
    basicComponent <- basicObject obj -< BasicObjectInput {
            boiNetwork = oiNetwork objInput
        }
    statsComponent <- objectStats obj -< ObjectStatsInput {
            osiNetwork = oiNetwork objInput
        }
    let stats = osoStats statsComponent
    rec projectileComponent <- trackingProjectile obj 1 -< TrackingProjectileInput {
                tpiAllCollisions = oiCollidingWith objInput,
                tpiAllObjects = oiAllObjects objInput,
                tpiSpeed = fromIntegral (stSpeed stats),
                tpiCurrPos = position
            }
        position <- (^+^ (goPos obj)) ^<< integral -< tpoMoveDelta projectileComponent
    -- Return state
    let (x, y) = vectorRoundedComponents position
    returnA -< (defaultObjOutput objInput) { 
        ooKillRequest = lMerge (booObjectDestroyed basicComponent) (tpoHitTargetEvent projectileComponent),
        ooGraphic = draw minionProjectileImage (Mask Nothing x y),
        ooGraphicLayer = GameLayer 6,
        ooGameObject = obj {
                goPos = position
            }
    }