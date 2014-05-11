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

data MapBlock = HeroSpawn -- map blueprint only
              | HeroBlock
              | Wall
              | Empty
              | MonsterSpawn -- map blueprint only
              | MonsterBlock -- map blueprint only
              | Treasure
              deriving (Bounded, Enum, Eq, Show)

type Game a = World -> IO a

data World = World {
    _whero :: Hero,
    _wmonsters :: [Monster],
    _wmap :: Map
} deriving (Show)

type Coords = (Integer, Integer)

data Hero = Hero {
    _hxpos :: Integer,
    _hypos :: Integer,
    _hhealth :: Integer
} deriving (Show)

data Monster = Monster {
    _mxpos :: Integer,
    _mypos :: Integer,
    _mtype :: MonsterType,
    _mhealth :: Integer
} deriving (Show)

data MonsterType
    = Monster1
     | Monster2
     | Monster3
     deriving (Show)

makeLenses ''Hero
makeLenses ''Monster
makeLenses ''World
