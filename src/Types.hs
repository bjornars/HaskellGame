{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Array.IArray

-- | Prefix directions with 'D' to not conflict with Either (Left, Right)
data Direction = DUp | DDown | DLeft | DRight
               deriving (Eq)

-- | Game Action
data GAction
    = Quit
    | Attack
    | Move Direction
    | ChangeLevel
    | None
    deriving (Eq)

type Map = Array (Integer, Integer) MapBlock

data MapBlock = HeroSpawn
              | Wall
              | Empty
              | MonsterSpawn
              | Monster
              | Treasure
              deriving (Bounded, Enum, Eq, Show)

type Game a = World -> IO a

data World = World {
    _whero :: Hero,
    _wmap :: Map
} deriving (Show)

data Hero = Hero {_hxpos :: Integer, _hypos :: Integer} deriving (Show)

makeLenses ''Hero
makeLenses ''World
