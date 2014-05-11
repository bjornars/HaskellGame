{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Input = InputC Char | InputS InputSymbol
data InputSymbol = IEscape

type Tick = (World -> Input -> IO (World, Bool))

type Map = [[MapBlock]]
type MapBlock = Char

data World = World {
    _whero :: Hero,
    _wmap :: Map
} deriving (Show)

data Hero = Hero {_hxpos :: Integer, _hypos :: Integer} deriving (Show)

makeLenses ''Hero
makeLenses ''World
