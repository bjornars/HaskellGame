{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

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

type Map = [[MapBlock]]
type MapBlock = Char

type Game a = World -> IO a

data World = World {
    _whero :: Hero,
    _wmap :: Map
} deriving (Show)

data Hero = Hero {_hxpos :: Integer, _hypos :: Integer} deriving (Show)

makeLenses ''Hero
makeLenses ''World
