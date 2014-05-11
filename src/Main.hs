module Main where

import Control.Exception.Base

import Console (setupTerminal, resetTerminal, getInput, draw)
import Game (startGame)

main :: IO ()
main = do
    setupTerminal
    handle handler (startGame draw getInput)
    resetTerminal

  where handler :: AsyncException -> IO ()
        handler _ = return ()
