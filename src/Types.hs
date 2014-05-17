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

type MapArray a = Array (Integer, Integer) a
type Map = MapArray MapBlock

data MapBlock = HeroSpawn -- map blueprint only
              | HeroBlock
              | Wall
              | Empty
              | Void
              | MonsterSpawn -- map blueprint only
              | MonsterBlock -- map blueprint only
              | Treasure
              deriving (Bounded, Enum, Eq, Show)

type Game a = GameState -> IO a

data GameState = GameState {
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
makeLenses ''GameState

class Renderable a where
    blockType :: a -> MapBlock
    coords :: a -> Coords

instance Renderable Hero where
    blockType _ = HeroBlock
    coords hero = (hero^.hxpos, hero^.hypos)

instance Renderable Monster where
    blockType _ = MonsterBlock
    coords monster = (monster^.mxpos, monster^.mypos)


(|-|), (|+|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)
