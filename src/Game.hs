module Game
( startGame
) where

import Control.Monad
import Control.Lens
import Data.Array.IArray

import Types
import GameMap

startGame :: Game () -> IO GAction -> IO ()
startGame draw getInput = do
    let gameMap = forceMap $ loadMap mapBlock1
        heroPos = head $ findBlocks HeroSpawn gameMap
        world = World { _whero = uncurry Hero heroPos, _wmap = gameMap}
    gameLoop draw getInput world
    return ()

gameLoop :: Game () -> IO GAction -> Game ()
gameLoop draw getAction world = do
    draw world
    action <- getAction
    unless (action == Quit) $ do
        tick action world >>= gameLoop draw getAction


tick :: GAction -> Game World
tick action world = do
    let world' = world & case action of
            Move DUp    -> (whero.hxpos) -~ 1
            Move DDown  -> (whero.hxpos) +~ 1
            Move DLeft  -> (whero.hypos) -~ 1
            Move DRight -> (whero.hypos) +~ 1
            _            -> id

    if validateAction world'
        then return world'
        else return world


validateAction :: World -> Bool
validateAction world = case (world^.wmap) ! idx of
        Empty        -> True
        Monster      -> True
        _            -> False
    where hero = world^.whero
          idx  = ((hero^.hxpos), (hero^.hypos))
