module Main where


import Console (setupTerminal, resetTerminal, getInput, draw)
import Game (gameLoop)
import Types


main :: IO ()
main = do
    setupTerminal
    let world = World { _hero = Hero 0 0 }
    draw world
    gameLoop world draw getInput

    resetTerminal
