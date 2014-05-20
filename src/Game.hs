module Game
( startGame
) where

import Control.Arrow
import Control.Monad
import Control.Lens hiding (Level)
import Data.Array.IArray
import Data.List
import Data.Ord

import Types
import Level

startGame :: Game () -> IO GAction -> IO ()
startGame draw getInput = do
    let gameLevel = fillVoid $ forceLevel $ loadLevel level1
        (newLevel, hero, monsters) = extractActorsFromLevel gameLevel
        world = GameState { _whero = hero, _wmap = newLevel, _wmonsters = monsters}

    gameLoop draw getInput world
    return ()


gameLoop :: Game () -> IO GAction -> Game ()
gameLoop draw getAction = go
    where
        go world = do
            draw world
            action <- getAction
            when (action /= Quit) $
                go $ tick action world


tick :: GAction -> GameState -> GameState
tick action world = let
        world' = world & case action of
            Move DUp    -> (whero.hxpos) -~ 1
            Move DDown  -> (whero.hxpos) +~ 1
            Move DLeft  -> (whero.hypos) -~ 1
            Move DRight -> (whero.hypos) +~ 1
            _            -> id
        in
    if validateAction world'
        then moveMonsters world'
        else world


validateAction :: GameState -> Bool
validateAction world = case (world^.wmap) ! idx of
        Empty        -> True
        -- Monster      -> True
        _            -> False
    where idx  = (world^.whero.hxpos, world^.whero.hypos)


extractActorsFromLevel :: Level -> (Level, Hero, [Monster])
extractActorsFromLevel level = (monsterlessLevel, makeHero heroes, makeMonsters monsters)
    where (heroes, herolessLevel) = splitOut HeroSpawn level
          (monsters, monsterlessLevel) = splitOut MonsterSpawn herolessLevel
          makeHero hs = uncurry Hero (fst.head $ hs) 20
          makeMonsters = map (\pos -> uncurry Monster (fst pos) Monster1 5)


splitOut :: Block -> Level -> ([(Coords, Block)], Level)
splitOut bType level = (blocks, remainingLevel)
    where blocks = findBlocks bType level
          remainingLevel =  level // map (second (const Empty)) blocks


moveMonsters :: GameState -> GameState
moveMonsters world = wmonsters %~ map moveMonster' $ world
    where m = world^.wmap
          h = world^.whero
          moveMonster' = moveMonster m h


moveMonster :: Level -> Hero -> Monster -> Monster
moveMonster level hero monster =
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
        if level ! (x,y) == Empty then
            monster { _mxpos = x, _mypos = y }
        else
            monster
