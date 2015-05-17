{-# LANGUAGE Arrows #-}

module Game.Server.Objects.GameSetup where

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry

import Network.Socket

import Numeric.IEEE

import Game.Shared.Object
import Game.Server.Object
import Game.Server.Objects.Network
import Game.Server.Objects.Towers
import Game.Server.Objects.Base

-- |Signal function which spawns all the required scene objects, then destroys itself
setupBasicGame :: Object
setupBasicGame = proc objInput -> do
    -- Create initial objects
    setupEvent <- delayEvent epsilon <<< now () -< ()
    -- Return initial objects
    returnA -< (defaultObjOutput objInput) {
            ooKillRequest = setupEvent,
            ooSpawnRequests = catEvents [setupEvent `tag` netManager,
                                         -- Top Turrets
                                         setupEvent `tag` turretObject (vector2 770 75) Top Blue, -- Blue top outer
                                         setupEvent `tag` turretObject (vector2 1400 75) Top Blue, -- Blue top inner
                                         setupEvent `tag` turretObject (vector2 2130 75) Top Blue, -- Blue top base
                                         setupEvent `tag` turretObject (vector2 75 810) Top Red, -- Red top outer
                                         setupEvent `tag` turretObject (vector2 75 1440) Top Red, -- Red top inner
                                         setupEvent `tag` turretObject (vector2 75 2170) Top Red, -- Red top base
                                         -- Middle turrets
                                         setupEvent `tag` turretObject (vector2 2400 550) Middle Blue, -- Blue mid base
                                         setupEvent `tag` turretObject (vector2 2100 850) Middle Blue, -- Blue mid inner
                                         setupEvent `tag` turretObject (vector2 1800 1150) Middle Blue, -- Blue mid outer
                                         setupEvent `tag` turretObject (vector2 550 2400) Middle Red, -- Red mid base
                                         setupEvent `tag` turretObject (vector2 850 2100) Middle Red, -- Red mid inner
                                         setupEvent `tag` turretObject (vector2 1150 1800) Middle Red, -- Red mid outer
                                         -- Bottom turrets
                                         setupEvent `tag` turretObject (vector2 2900 800) Bottom Blue, -- Blue bot base
                                         setupEvent `tag` turretObject (vector2 2900 1430) Bottom Blue, -- Blue bot inner
                                         setupEvent `tag` turretObject (vector2 2900 2060) Bottom Blue, -- Blue bot outer
                                         setupEvent `tag` turretObject (vector2 840 2900) Bottom Red, -- Red bot base
                                         setupEvent `tag` turretObject (vector2 1470 2900) Bottom Red, -- Red bot inner
                                         setupEvent `tag` turretObject (vector2 2100 2900) Bottom Red, -- Red bot outer
                                         -- Nexus
                                         setupEvent `tag` nexusObject (vector2 2700 250) Blue, -- Blue nexus
                                         setupEvent `tag` nexusObject (vector2 250 2700) Red -- Red nexus
                                         -- Bases
                                         -- TODO
                                         ],
            ooConsole = event [] (const "Setting up game") setupEvent
        }

