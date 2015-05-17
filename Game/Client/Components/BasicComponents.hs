{-# LANGUAGE Arrows #-}

module Game.Client.Components.BasicComponents where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Data.Maybe
import Data.Fixed

import Numeric.IEEE

import Game.Shared.Types
import Game.Shared.Networking
import Game.Shared.Object
import Game.Shared.Arrows
import Game.Client.UI
import Game.Client.Object
import Game.Client.Resources
import Game.Client.Input
import Game.Client.Graphics
import Game.Client.Networking

------------------
-- Basic object --
------------------

-- |Input structure for the basic object component
data BasicObjectInput = BasicObjectInput {
        boiNetwork :: NetworkInput -- ^List of network input
    }

-- |Output structure for the basic object component
data BasicObjectOutput = BasicObjectOutput {
        booObjectDestroyed :: Yampa.Event () -- ^Event fired when the object should be destroyed
       ,booPosition :: Vector2D -- ^The objects new position
    }

-- |Basic component that every game object should use
basicObject :: GameObject -- ^Game object that this component will manage
            -> SF BasicObjectInput BasicObjectOutput
basicObject obj = proc input -> do
    -- Network events
    destroyedEvent <- destroyObject (goId obj) -< boiNetwork input
    moveEvent <- objectMoved (goId obj) -< boiNetwork input
    -- Linear interpolation of movement
    currTime <- localTime -< ()
    moveEventTime <- hold 0 -< moveEvent `tag` currTime
    currentPosition <- hold initPos -< moveEvent
    previousPosition <- hold initPos <<< staggerEvent 1 -< moveEvent
    let position = lerpV previousPosition currentPosition (min 1.0 ((currTime - moveEventTime) * 5))
    -- Return state
    returnA -< BasicObjectOutput { 
        booObjectDestroyed = destroyedEvent,
        booPosition = position
    } 
    where
        initPos = goPos obj
    
------------------
-- Object stats --
------------------ 

-- |Input structure for the stats component
data ObjectStatsInput = ObjectStatsInput {
        osiNetwork :: NetworkInput -- ^The network input
    }

-- |Output structure for the stats component
data ObjectStatsOutput = ObjectStatsOutput {
        osoHealthChanged :: Yampa.Event Int -- ^Event for when the health stat is changed
       ,osoMaxHealthChanged :: Yampa.Event Int -- ^Event for when the max health stat is changed
       ,osoManaChanged :: Yampa.Event Int -- ^Event for when the mana stat is changed
       ,osoMaxManaChanged :: Yampa.Event Int -- ^Event for when the max mana stat is changed
       ,osoSpeedChanged :: Yampa.Event Int -- ^Event for when the speed stat is changed
       ,osoDefenceChanged :: Yampa.Event Int -- ^Event for when the defence stat is changed
       ,osoAttackChanged :: Yampa.Event Int -- ^Event for when the attack stat is changed
       ,osoDeathEvent :: Yampa.Event () -- ^Event for the object has died
       ,osoStats :: Stats -- ^Structure containing the updated stats
    } 

-- |Component that maintains the statistics data for a game object
objectStats :: GameObject -- ^The game object this component should manage
            -> SF ObjectStatsInput ObjectStatsOutput
objectStats obj = proc input -> do
    -- Get all update events
    healthEvent <- objectStatUpdated (goId obj) Health -< osiNetwork input
    maxHealthEvent <- objectStatUpdated (goId obj) MaxHealth -< osiNetwork input
    manaEvent <- objectStatUpdated (goId obj) Mana -< osiNetwork input
    maxManaEvent <- objectStatUpdated (goId obj) MaxMana -< osiNetwork input
    speedEvent <- objectStatUpdated (goId obj) Speed -< osiNetwork input
    defenceEvent <- objectStatUpdated (goId obj) Defence -< osiNetwork input
    attackEvent <- objectStatUpdated (goId obj) Attack -< osiNetwork input
    -- Update the objects stats on update stats net messages
    health <- hold (stHealth startSt) -< healthEvent
    maxHealth <- hold (stMaxHealth startSt) -< maxHealthEvent
    mana <- hold (stMana startSt) -< manaEvent
    maxMana <- hold (stMaxMana startSt) -< maxManaEvent
    speed <- hold (stSpeed startSt) -< speedEvent
    defence <- hold (stDefence startSt) -< defenceEvent
    attack <- hold (stAttack startSt) -< attackEvent
    -- Read in death events
    deathEvent <- killedObject (goId obj) -< osiNetwork input
    -- Return updated stats and events
    returnA -< ObjectStatsOutput {
        osoHealthChanged = healthEvent,
        osoMaxHealthChanged = maxHealthEvent,
        osoManaChanged = manaEvent,
        osoMaxManaChanged = maxManaEvent,
        osoSpeedChanged = speedEvent,
        osoDefenceChanged = defenceEvent,
        osoAttackChanged = attackEvent,
        osoDeathEvent = deathEvent,
        osoStats = Stats {
                stHealth = health,
                stMaxHealth = maxHealth,
                stMana = mana,
                stMaxMana = maxMana,
                stSpeed = speed,
                stDefence = defence,
                stAttack = attack
            }
        }
    where
        startSt = fromJust (goStats obj)
        
-----------------------
-- Healthbar display --
-----------------------

-- |Input structure for a healthbar display 
data HealthbarDisplayInput = HealthbarDisplayInput {
        hdiHealthChangedEvent :: Yampa.Event Int -- ^Event for when the objects health has changed
       ,hdiObjectPosition :: Vector2D -- ^The current position of the object
       ,hdiCurrentHealth :: Int -- ^The current helath of the object
       ,hdiCurrentMaxHealth :: Int -- ^The current maximum health of the object
    }

-- |Output structure for a healthbar display
data HealthbarDisplayOutput = HealthbarDisplayOutput {
        hdoHealthbarGraphic :: Graphic -- ^The healthbar graphic
    } 

-- |Displays a healthbar above a game object for a fixed amount of time
-- whenever the health of the object has been changed
healthbarDisplay :: Time -- ^The amount of time to display the healthbar for
                 -> Vector2D -- ^The offset to display the healthbar at
                 -> Int -- ^The width of the healthbar
                 -> Int -- ^The height of the healthbar
                 -> SF HealthbarDisplayInput HealthbarDisplayOutput
healthbarDisplay displayTime offset width height = proc input -> do
    -- Do we need to display the bar?
    timerTick <- repeatedly 1.0 () -< ()
    rec displayTimer <- dAccumHold 0 -< lMerge (hdiHealthChangedEvent input `tag` (const displayTime)) 
                                               (timerTick `tag` (const (max 0 (displayTimer - 1))))
    let display = displayTimer > 0
    -- Create the healthbar
    let playerHp = fromIntegral (hdiCurrentHealth input)
        playerMaxHp = fromIntegral (hdiCurrentMaxHealth input)
        rect = rectangle ((hdiObjectPosition input) ^+^ offset) width height
        healthBar = drawProgressBar rect (SDL.Color 64 64 64) (SDL.Color 255 0 0) 1 (playerHp / playerMaxHp)
    -- Return the graphic
    returnA -< HealthbarDisplayOutput {
            hdoHealthbarGraphic = if display 
                then healthBar
                else emptyG
        }