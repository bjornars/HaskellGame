{-# LANGUAGE ExistentialQuantification, RankNTypes, TemplateHaskell #-}

module Types where

import Control.Lens hiding (Level)
import Data.Array.IArray
import Graphics.Vty

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

type Coords = (Integer, Integer)
type LevelArray a = Array Coords a
type Level = LevelArray Block


data Undefined = Undefined

data MonsterState = Living | Dead
                  deriving (Show)

type Monsters = [MonsterW]
data MonsterW = forall a . Monster a => MkMonster a

class Monster a where
    mImage :: a -> Image
    mHurt :: a -> a
    mTick :: a -> a
    mState :: a -> MonsterState
    mPos :: a -> Coords


instance Monster MonsterW where
    mImage (MkMonster m) = mImage m
    mHurt (MkMonster m) = MkMonster $ mHurt m
    mTick (MkMonster m) = MkMonster $ mTick m
    mState (MkMonster m) = mState m
    mPos (MkMonster m) = mPos m


data Block = Wall
           | Empty
           | Void
           | Treasure
           deriving (Bounded, Enum, Eq, Show)

type Game a = Monster m => GameState m -> IO a

data Monster m => GameState m = GameState {
    _whero :: Hero,
    _wmonsters :: [m],
    _wmap :: Level
} deriving (Show)


data Hero = Hero {
    _hxpos :: Integer,
    _hypos :: Integer,
    _hhealth :: Integer
} deriving (Show)

mkHero :: Coords -> Hero
mkHero (y, x) = Hero x y 100

makeLenses ''Hero
makeLenses ''GameState

(|-|), (|+|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)
