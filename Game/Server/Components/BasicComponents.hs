{-# LANGUAGE Arrows #-}

module Game.Server.Components.BasicComponents where

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
import Game.Server.Networking

------------------
-- Basic object --
------------------

-- |Input structure for a basic game object
data BasicObjectInput = BasicObjectInput {
        boiNetwork :: ServerNetInput -- ^Network input
    }

-- |Output structure for a basic game object
data BasicObjectOutput = BasicObjectOutput {
        booGlobalAnnounceEvent :: Yampa.Event () -- ^Event fired when the object should announce itself to all connected clients
       ,booSocketAnnounceEvent :: Yampa.Event [Socket] -- ^Event for when the object should announce itself to a client
    }

-- |Base component that all objects should use. Informs the object when it should
-- send Create messages to newly connected clients
basicObject :: SF BasicObjectInput BasicObjectOutput
basicObject = proc input -> do
    -- Connection events
    announceEvent <- delayEvent epsilon <<< now () -< ()
    newClientEvent <- clientsConnected -< boiNetwork input
    -- Return state
    returnA -< BasicObjectOutput { 
        booGlobalAnnounceEvent = announceEvent,
        booSocketAnnounceEvent = newClientEvent
    }   
    
------------------------
-- Basic owned object --
------------------------

-- |Input structure for the owned basic object
data BasicOwnedObjectInput = BasicOwnedObjectInput {
        booiNetwork :: ServerNetInput -- ^Network input
    }

-- |Output structure for the owned basic object
data BasicOwnedObjectOutput = BasicOwnedObjectOutput {
        boooGlobalAnnounceEvent :: Yampa.Event () -- ^Event fired when the object should announce itself to all connected clients
       ,boooSocketAnnounceEvent :: Yampa.Event [Socket] -- ^Event for when the object should announce itself to a client
       ,boooOwnerDisconnected :: Yampa.Event () -- ^Event fired when the object owned has disconnected
       ,boooPositionChanged :: Yampa.Event Vector2D -- ^Event for when the objects position has changed
    }

-- |Base component for an object which is owned by a connected client. Owned objects
-- can be moved by their owner, and are removed when their owner disconnects
basicOwnedObject :: Socket -- ^Socket of the parent client
                 -> SF BasicOwnedObjectInput BasicOwnedObjectOutput
basicOwnedObject sock = proc input -> do
    -- Connection events
    announceEvent <- delayEvent epsilon <<< now () -< ()
    disconnectedEvent <- clientDisconnected sock -< booiNetwork input
    newClientEvent <- clientsConnected -< booiNetwork input
    -- Position
    moveRequestEvent <- moveRequest sock -< booiNetwork input
    -- Return state
    returnA -< BasicOwnedObjectOutput { 
        boooGlobalAnnounceEvent = announceEvent,
        boooSocketAnnounceEvent = newClientEvent,
        boooOwnerDisconnected = disconnectedEvent,
        boooPositionChanged = moveRequestEvent
    } 

------------------
-- Object stats --
------------------  

-- |Input structure for the stats component
data ObjectStatsInput = ObjectStatsInput {
        osiHealthChanged :: Yampa.Event Int -- ^Event to request a change to the health value
       ,osiMaxHealthChanged :: Yampa.Event Int -- ^Event to request a change to the max health value
       ,osiManaChanged :: Yampa.Event Int -- ^Event to request a change to the mana value
       ,osiMaxManaChanged :: Yampa.Event Int -- ^Event to request a change to the max mana value
       ,osiSpeedChanged :: Yampa.Event Int -- ^Event to request a change to the speed value
       ,osiDefenceChanged :: Yampa.Event Int -- ^Event to request a change to the defence value
       ,osiAttackChanged :: Yampa.Event Int -- ^Event to request a change to the attack value
    }

-- |Output structure for the stats component
data ObjectStatsOutput = ObjectStatsOutput {
        osoHealthChanged :: Yampa.Event Int -- ^Event for when the health value has been changed
       ,osoMaxHealthChanged :: Yampa.Event Int -- ^Event for when the max health value has been changed
       ,osoManaChanged :: Yampa.Event Int -- ^Event for when the mana value has been changed
       ,osoMaxManaChanged :: Yampa.Event Int -- ^Event for when the max mana value has been changed
       ,osoSpeedChanged :: Yampa.Event Int -- ^Event for when the speed value has been changed
       ,osoDefenceChanged :: Yampa.Event Int -- ^Event for when the defence value has been changed
       ,osoAttackChanged :: Yampa.Event Int -- ^Event for when the attack value has been changed
       ,osoDeathEvent :: Yampa.Event () -- ^Event fired when the object is killed
       ,osoStats :: Stats -- ^Object stats structure
    }  

-- |Component that manages the stats for an object
objectStats :: Stats -- ^Initial values for the object stats
            -> SF ObjectStatsInput ObjectStatsOutput
objectStats startSt = proc input -> do
    -- Hold and update stats when necessary 
    rec health <- accumBy (+) (stHealth startSt) <<< delayEvent epsilon -< clampEvent (osiHealthChanged input) healthValue maxHealthValue
        maxHealth <- accumBy (+) (stMaxHealth startSt) -< osiMaxHealthChanged input
        mana <- accumBy (+) (stMana startSt)  <<< delayEvent epsilon -< clampEvent (osiManaChanged input) manaValue maxManaValue
        maxMana <- accumBy (+) (stMaxMana startSt) -< osiMaxHealthChanged input
        speed <- accumBy (+) (stSpeed startSt) -< osiSpeedChanged input
        defence <- accumBy (+) (stDefence startSt) -< osiDefenceChanged input
        attack <- accumBy (+) (stAttack startSt) -< osiAttackChanged input
        healthValue <- hold (stHealth startSt) -< health
        maxHealthValue <- hold (stMaxHealth startSt) -< maxHealth
        manaValue <- hold (stMana startSt) -< mana
        maxManaValue <- hold (stMaxMana startSt) -< maxMana
        speedValue <- hold (stSpeed startSt) -< speed
        defenceValue <- hold (stDefence startSt) -< defence
        attackValue <- hold (stAttack startSt) -< attack
    -- Return updated stats
    returnA -< ObjectStatsOutput { 
        osoHealthChanged = health,
        osoMaxHealthChanged = maxHealth,
        osoManaChanged = mana,
        osoMaxManaChanged = maxMana,
        osoSpeedChanged = speed,
        osoDefenceChanged = defence,
        osoAttackChanged = attack,
        osoDeathEvent = condEvent (healthValue <= 0),
        osoStats = Stats {
                stHealth = healthValue,
                stMaxHealth = maxHealthValue,
                stMana = manaValue,
                stMaxMana = maxManaValue,
                stSpeed = speedValue,
                stAttack = attackValue,
                stDefence = defenceValue
            }
    } 
    where 
        clampEvent evt@(Yampa.Event val) currVal maxVal | newVal < maxVal = evt
                                                        | (newVal > maxVal) && (currVal /= maxVal) = Yampa.Event (maxVal - currVal)
                                                        | newVal < 0 = Yampa.Event (-currVal)
                                                        | otherwise = noEvent
            where 
                newVal = val + currVal
        clampEvent _ _ _ = noEvent
        