module Game
( startGame
) where

import Control.Monad
import Control.Lens

import Types
import GameMap

type Game a = World -> IO a

startGame :: (World -> IO ()) -> IO GAction -> IO ()
startGame draw getInput = do
    let gameMap = forceMap mapBlock1
        heroPos = head $ findBlocks mbHeroSpawn gameMap
        world = World { _whero = uncurry Hero heroPos, _wmap = gameMap}
    gameLoop draw getInput world
    return ()

gameLoop :: (World -> IO ()) -> IO GAction -> Game ()
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
validateAction world = case world^.wmap.atHero of
        '.'          -> True
        '#'          -> True
        _            -> False
    where hero = world^.whero
          atHero = to $ index' (fromInteger $ hero^.hxpos) (fromInteger $ hero^.hypos)
          index' x y map' = map' !! y !! x
