module Game
( startGame
) where

import Control.Monad
import Control.Lens

import Types
import GameMap


startGame :: (World -> IO ()) -> IO Input -> IO ()
startGame draw getInput = do
    let gameMap = forceMap mapBlock1
    let heroPos = head $ findBlocks mbHeroSpawn gameMap

    let world = World { _whero = uncurry Hero heroPos, _wmap = gameMap}
    draw world
    gameLoop world draw getInput

gameLoop :: World -> (World -> IO ()) -> IO Input -> IO ()
gameLoop world draw getInput = go world
    where go w = do
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

    -- only do the move if it is allowed.
    let world'' = case getMoveType world' of
            MTMove -> world'
            _      -> world

    let done = case input of
            (InputS IEscape) -> True
            (InputC 'q') -> True
            _ -> False

    return (world'', done)


getMoveType :: World -> MoveType
getMoveType world = case world^.wmap.atHero of
        -- mbEmpty   -> MTMove
        -- mbMonster -> MTAttack
        -- TODO: why is ghc-mod check complaining about this?
        '.'          -> MTMove
        '#'          -> MTAttack
        _            -> MTInvalid

    where hero = world^.whero
          atHero = to $ index' (fromInteger $ hero^.hxpos) (fromInteger $ hero^.hypos)
          index' x y map' = map' !! y !! x
