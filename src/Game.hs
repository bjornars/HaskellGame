module Game
( startGame
) where

import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Array.IArray
import Data.List
import Data.Ord

import Types
import GameMap

startGame :: Game () -> IO GAction -> IO ()
startGame draw getInput = do
    let gameMap = forceMap $ loadMap mapBlock1
        (newMap, hero, monsters) = extractActorsFromMap gameMap
        world = World { _whero = hero, _wmap = newMap, _wmonsters = monsters}
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
        then moveMonsters world'
        else world


validateAction :: World -> Bool
validateAction world = case (world^.wmap) ! idx of
        Empty        -> True
        -- Monster      -> True
        _            -> False
    where idx  = (world^.whero.hxpos, world^.whero.hypos)

extractActorsFromMap :: Map -> (Map, Hero, [Monster])
extractActorsFromMap map' = (monsterlessMap, makeHero heroes, makeMonsters monsters)
    where (heroes, herolessMap) = splitOut HeroSpawn map'
          (monsters, monsterlessMap) = splitOut MonsterSpawn herolessMap
          makeHero hs = uncurry Hero (fst.head $ hs) 20
          makeMonsters = map (\pos -> uncurry Monster (fst pos) Monster1 5)

splitOut :: MapBlock -> Map -> ([((Integer, Integer), MapBlock)], Map)
splitOut bType map' = (blocks, remainingMap)
    where blocks = findBlocks bType map'
          remainingMap =  map' // map (second (const Empty)) blocks

moveMonsters :: World -> World
moveMonsters world = over wmonsters (map (moveMonster (world^.wmap) (world^.whero))) world

(|-|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)

moveMonster :: Map -> Hero -> Monster -> Monster
moveMonster map' hero monster =
    let nfst = negate . fst
        nsnd = negate . snd
        vector = coords hero |-| coords monster
        directions = [
               (nfst vector, first succ),
               ( fst vector, first pred),
               (nsnd vector, second succ),
               ( snd vector, second pred)]
        move = snd.head $ sortBy (comparing fst) directions
        (x, y) = move $ coords monster
    in
        if map' ! (x,y) == Empty then
            monster { _mxpos = x, _mypos = y }
        else
            monster

