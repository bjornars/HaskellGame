module Game where

import Control.Monad
import Control.Lens

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
    let world' = world & case input of
            (InputC 'a') -> (whero.hxpos) -~ 1
            (InputC 'd') -> (whero.hxpos) +~ 1
            (InputC 'w') -> (whero.hypos) -~ 1
            (InputC 's') -> (whero.hypos) +~ 1
            _            -> id

    let done = case input of
            (InputS IEscape) -> True
            (InputC 'q') -> True
            _ -> False

    return (world', done)
