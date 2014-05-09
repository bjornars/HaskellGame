{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Input = InputC Char | InputS InputSymbol
data InputSymbol = IEscape

type Tick = (World -> Input -> IO (World, Bool))

data World = World { _whero :: Hero } deriving (Show)

data Hero = Hero {_hxpos :: Int, _hypos :: Int} deriving (Show)

makeLenses ''Hero
makeLenses ''World
