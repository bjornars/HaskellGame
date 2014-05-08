module Main where

import Control.Exception.Base

import Console (setupTerminal, resetTerminal, getInput, draw)
import Game (gameLoop)
import Types


main :: IO ()
main = do
    setupTerminal
    let world = World { _hero = Hero 0 0 }
    draw world

    handle handler (gameLoop world draw getInput)
    resetTerminal

  where handler :: AsyncException -> IO ()
        handler _ = return ()
