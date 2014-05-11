module Main where

import Control.Exception.Base
import Graphics.Vty (mkVty, shutdown)

import Console
import Game

main :: IO ()
main = do
    vty <- mkVty
    let action = getAction defaultKeys vty
        update = draw vty
    handle handler $ startGame update action
    shutdown vty
  where handler :: AsyncException -> IO ()
        handler _ = putStrLn "Uh oh!"
