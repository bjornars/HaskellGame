module Main where

import Control.Exception.Base
import Graphics.Vty (mkVty, shutdown)
import Data.Default (def)

import Console
import Game

main :: IO ()
main = do
    vty <- mkVty def
    let action = getAction defaultKeys vty
        update = draw vty

    bracket_ (return ()) (shutdown vty)
        $ startGame update action
