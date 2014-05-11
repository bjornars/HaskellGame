module Main where

import Control.Exception.Base

import Console (setupTerminal, resetTerminal, getInput, draw)
import Game

main :: IO ()
main = do
    setupTerminal
    handle handler (gameLoop defaultWorld draw getInput)
    resetTerminal

  where handler :: AsyncException -> IO ()
        handler _ = return ()
