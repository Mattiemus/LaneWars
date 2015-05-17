{-# LANGUAGE Arrows #-}

module Game.Client.Objects.UI where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Data.Maybe

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.Objects.Towers
import Game.Client.Object
import Game.Client.Input
import Game.Client.UI
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

-- |Game object responsible for managing the client UI. Will draw the health and mana bars, as well 
-- as the two selectable attacks, highlighting the currently active attack
uiManager :: Object
uiManager = proc objInput -> do
    -- Get the players stats
    let player = maybe basicGameObject (id) (findObjByType ControlledPlayer (oiAllObjects objInput))
        playerStats = maybe basicStats (id) (goStats player)
        playerTeam = goTeam player
        playerHp = fromIntegral (stHealth playerStats) 
        playerMaxHp = fromIntegral (stMaxHealth playerStats)
        playerMana = fromIntegral (stMana playerStats) 
        playerMaxMana = fromIntegral (stMaxMana playerStats)
    -- Handle attack selection
    key1Pressed <- keyDown SDL.SDLK_1 -< oiInput objInput
    key2Pressed <- keyDown SDL.SDLK_2 -< oiInput objInput
    currentSelection <- hold 1 -< lMerge (key1Pressed `tag` 1) (key2Pressed `tag` 2)
    -- Game over network message
    winningTeamEvent <- teamWon -< oiNetwork objInput
    winningTeam <- hold Neutral -< winningTeamEvent
    -- Create ui components
    let healthBar = drawProgressBar (Rect 262 718 500 24) (SDL.Color 64 64 64) (SDL.Color 255 0 0) 2 (playerHp / playerMaxHp)
        manaBar = drawProgressBar (Rect 262 743 500 24) (SDL.Color 64 64 64) (SDL.Color 25 25 122) 2 (playerMana / playerMaxMana)
        icon1 = draw (attackIconImage 1 (currentSelection == 1)) (Mask Nothing 262 663)
        icon2 = draw (attackIconImage 2 (currentSelection == 2)) (Mask Nothing 315 663)
        victory = if winningTeam == Neutral
            then emptyG
            else draw (victoryImage (playerTeam == winningTeam)) (Mask Nothing 256 128)
    returnA -< (defaultObjOutput objInput) {
            ooGraphic = drawAll [healthBar, manaBar, icon1, icon2, victory],
            ooGraphicLayer = UILayer
        }