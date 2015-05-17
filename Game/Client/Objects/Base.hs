{-# LANGUAGE Arrows #-}

module Game.Client.Objects.Base where

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
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-- |Game object for a player base
nexusObject :: GameObject -- ^Object representation sent by server
            -> Object
nexusObject obj = proc objInput -> do
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
    healthbarComponent <- healthbarDisplay 5 (vector2 26 (-12)) 76 6 -< HealthbarDisplayInput {
            hdiHealthChangedEvent = osoHealthChanged statsComponent,
            hdiObjectPosition = position,
            hdiCurrentHealth = stHealth stats,
            hdiCurrentMaxHealth = stMaxHealth stats
        }
    -- Return state
    let (x, y) = vectorRoundedComponents position
        team = goTeam obj
        health = stHealth stats
        maxHealth = stMaxHealth stats
        healthRep = round ((fromIntegral health / fromIntegral maxHealth) * 6)
    returnA -< (defaultObjOutput objInput) { 
        ooKillRequest = booObjectDestroyed basicComponent,
        ooGraphic = drawAll [draw (nexusImage team healthRep) (Mask Nothing x y),
                             hdoHealthbarGraphic healthbarComponent],
        ooGraphicLayer = GameLayer 5,
        ooGameObject = obj {
                goPos = position,
                goStats = Just stats
            }
    }