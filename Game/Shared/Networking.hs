{-# LANGUAGE DeriveGeneric #-}

module Game.Shared.Networking where

import GHC.Generics

import Control.Concurrent.MVar

import Data.Serialize
import Data.List

import IdentityList

import Game.Shared.Types
import Game.Shared.Object

-------------------
-- Network types --
-------------------
-- |Enumeration of network message structures
data NetMessage = MoveMe Vector2D     
                | PlayerData ILKey GameTeam PlayerClass
                | Kill ILKey
                | Create GameObject
                | Destroy ILKey
                | Move ILKey Vector2D
                | UpdateStats ILKey StatName Int
                | PlayerAttack Int Vector2D
                | TeamWon GameTeam
    deriving(Show, Eq, Generic)
instance Serialize NetMessage

-- |Network event enumeration
data NetEvent = Message NetMessage
              | ConnectionSuccess
              | ConnectionFailed
              | Disconnected
    deriving(Show, Eq)

-- |Type alias for a list of network events
type NetworkInput = [NetEvent]