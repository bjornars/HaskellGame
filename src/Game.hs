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
        world = World { _whero = uncurry Hero heroPos 10, _wmap = gameMap, _wmonsters = []}
    gameLoop draw getInput world
    return ()

gameLoop :: Game () -> IO GAction -> Game ()
gameLoop draw getAction world = do
    draw world
    action <- getAction
    unless (action == Quit) $ tick action world >>= gameLoop draw getAction


tick :: GAction -> Game World
tick action world = do
    let world' = world & case action of
            Move DUp    -> (whero.hxpos) -~ 1
            Move DDown  -> (whero.hxpos) +~ 1
            Move DLeft  -> (whero.hypos) -~ 1
            Move DRight -> (whero.hypos) +~ 1
            _            -> id

    return $ if validateAction world'
        then world'
        else world


validateAction :: World -> Bool
validateAction world = case (world^.wmap) ! idx of
        Empty        -> True
        -- Monster      -> True
        _            -> False
    where idx  = (world^.whero.hxpos, world^.whero.hypos)
