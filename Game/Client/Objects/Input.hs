{-# LANGUAGE Arrows #-}

module Game.Client.Objects.Input where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import IdentityList

import Numeric.IEEE

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.Objects.Towers
import Game.Client.Components.BasicComponents
import Game.Client.Object
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-- |Game object that translates keyboard input into camera movement
cameraManager :: Object
cameraManager = proc objInput -> do
    -- Keyboard input
    keyboardInput <- keyboardMovement 1000 -< oiInput objInput
    keyboardDelta <- integral -< keyboardInput
    -- Return state
    returnA -< (defaultObjOutput objInput) { 
            ooCameraDelta = keyboardDelta ^-^ (oiCamera objInput)
        }

-- |Game object for the players representation in the world.
-- Doesnt actually create the player straight away, instead sends a request
-- to the server to create the clients player avatar.
playerObject :: Object
playerObject = proc objInput -> do
    -- Announced data
    playerDataEvent <- playerData -< oiNetwork objInput
    -- Object to create
    let (id, team, playerClass) = fromEvent playerDataEvent
        obj = basicGameObject {
                goId = id,
                goType = ControlledPlayer,
                goTeam = team,
                goClass = playerClass,
                goSize = vector2 32 32,
                goStats = Just (Stats {
                    stHealth = 350,
                    stMaxHealth = 350,
                    stMana = 375,
                    stMaxMana = 375,
                    stSpeed = 300,
                    stDefence = 0,
                    stAttack = 0
                })
            }
    -- Return
    returnA -< (defaultObjOutput objInput) { 
        ooSpawnRequests = playerDataEvent `tag` [initialisedPlayerObject obj]
    } 
   
-- |Game object for an initialised player. Created by the "playerObject" function
-- upon the server sending the player data.
initialisedPlayerObject :: GameObject -- ^Game object sent by the server that represents the player
                        -> Object
initialisedPlayerObject obj = proc objInput -> do
    -- Components
    statsComponent <- objectStats obj -< ObjectStatsInput {
            osiNetwork = oiNetwork objInput
        }
    let stats = osoStats statsComponent
    -- Movement
    rec mousePosition <- mouseClickPosition SDL.ButtonLeft -< oiInput objInput
        mouseEvent <- mouseClick SDL.ButtonLeft -< oiInput objInput
        cameraPosAtClick <- hold spawnPosition -< mouseEvent `tag` (oiCamera objInput)
        let targetPos = mousePosition ^+^ cameraPosAtClick
            moveVector = targetPos ^-^ position
            moveDelta = if vectorDistance zeroVector moveVector < 30
                            then zeroVector
                            else (fromIntegral (stSpeed stats)) *^ (normalize moveVector)
        posUpdateEvent <- repeatedly (1.0 / 5.0) () -< ()
        positionDelta <- integral -< moveDelta
        positionOffset <- dAccumHoldBy (^+^) spawnPosition -< (osoDeathEvent statsComponent) `tag` (spawnPosition ^-^ position)
        let position = positionDelta ^+^ positionOffset
    -- Attacks
    rec mouseAttackPosition <- mouseClickPosition SDL.ButtonRight -< oiInput objInput
        mouseAttackEvent <- mouseClick SDL.ButtonRight -< oiInput objInput
        cameraPosAtAttackClick <- hold spawnPosition -< mouseAttackEvent `tag` (oiCamera objInput)
    let attackTargetPos = mouseAttackPosition ^+^ cameraPosAtAttackClick
        attackVector = attackTargetPos ^-^ position
        attackDelta = if attackVector == zeroVector
                        then zeroVector
                        else normalize attackVector
    key1Pressed <- keyDown SDL.SDLK_1 -< oiInput objInput
    key2Pressed <- keyDown SDL.SDLK_2 -< oiInput objInput
    currentAttackSelection <- hold 1 -< lMerge (key1Pressed `tag` 1) (key2Pressed `tag` 2)
    -- Components    
    healthbarComponent <- healthbarDisplay 1.5 (vector2 (-6) (-12)) 44 6 -< HealthbarDisplayInput {
            hdiHealthChangedEvent = osoHealthChanged statsComponent,
            hdiObjectPosition = position,
            hdiCurrentHealth = stHealth stats,
            hdiCurrentMaxHealth = stMaxHealth stats
        }
    -- Object state
    let (x, y) = vectorRoundedComponents position
        team = goTeam obj
    -- Used to reset camera on death
    cameraReset <- accumHoldBy (^+^) zeroVector -< (osoDeathEvent statsComponent) `tag` (oiCamera objInput)
    -- Return
    returnA -< (defaultObjOutput objInput) { 
        ooGraphic = drawAll [draw (playerImage team) (Mask Nothing x y),
                             hdoHealthbarGraphic healthbarComponent],
        ooGraphicLayer = GameLayer 10,
        ooMessages = catEvents [posUpdateEvent `tag` MoveMe position,
                                mouseAttackEvent `tag` PlayerAttack currentAttackSelection attackDelta],
        ooCameraDelta = spawnPosition ^-^ vector2 512 384 ^-^ cameraReset,
        ooGameObject = obj {
                goPos = position,
                goStats = Just stats
            }
    } 
    where
        spawnPosition | goTeam obj == Red = vector2 100 2900
                      | goTeam obj == Blue  = vector2 2900 100