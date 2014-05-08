module Types where

data Input = InputC Char | InputS InputSymbol
data InputSymbol = IEscape

type Tick = (World -> Input -> IO (World, Bool))

data World = World { _hero :: Hero }

data Hero = Hero {_xpos :: Int, _ypos :: Int}
