module Game where

import Control.Monad

import Types

gameLoop :: World -> (World-> IO ()) -> IO Input -> IO ()
gameLoop world draw getInput = go world where
    go w = do
         input <- getInput
         (w', done) <- tick w input
         draw w'
         unless done $ go w'


tick :: World -> Input -> IO (World, Bool)
tick world input = do
    let hero = _hero world

    let world' = case input of
            (InputC 'a') -> world { _hero = hero {_xpos = _xpos hero - 1 }}
            (InputC 'd') -> world { _hero = hero {_xpos = _xpos hero + 1 }}
            (InputC 'w') -> world { _hero = hero {_ypos = _ypos hero - 1 }}
            (InputC 's') -> world { _hero = hero {_ypos = _ypos hero + 1 }}
            _            -> world

    let done = case input of
            (InputS IEscape) -> True
            (InputC 'q') -> True
            _ -> False

    return (world', done)
