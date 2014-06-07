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
import qualified Levels.Level1 as L1

startGame :: Game () -> IO GAction -> IO ()
startGame draw getInput =
    let (level, hero, monsters) = L1.level
        world = GameState { _whero = hero, _wmap = level, _wmonsters = monsters }
    in gameLoop draw getInput world


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


moveMonsters :: GameState -> GameState
moveMonsters world =
    let monsters = world^.wmonsters
        hero = world^.whero
        level = (world^.wmap) // [coords &&& blockType $ hero]
        (_, monsters') = moveAndAddMonsters level hero monsters in
    set wmonsters monsters' world


-- can this be written as a fold?
moveAndAddMonsters :: Level -> Hero -> [Monster] -> (Level, [Monster])
moveAndAddMonsters level _ [] = (level, [])
moveAndAddMonsters level hero (m:ms) =
    let (level', m') = moveAndAddMonster level hero m
        (level'', ms') = moveAndAddMonsters level' hero ms in
    (level'', m': ms')


-- move a monsters according to 'AI', and then add it to our working map
moveAndAddMonster :: Level -> Hero -> Monster -> (Level, Monster)
moveAndAddMonster level hero monster =
    let newMonster = moveMonster level hero monster
        addMonster m l = l // [coords &&& blockType $ m]
        in
    (addMonster newMonster level, newMonster)

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
