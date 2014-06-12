{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Arrow ((&&&))
import Control.Monad.Operational
import Data.Array.IArray hiding (range)
import System.Random (randomRIO)
import Data.Sequence ((|>), (<|), fromList, Seq)
import Data.Foldable (toList)
import qualified Data.Sequence as S

import Types
import qualified Levels.Level1 as L1

startGame :: (Level -> IO ()) -> IO GAction -> IO ()
startGame draw getInput =
    let (level, actors) = L1.level in
    go level $ fromList (drawActor:actors)
    where
    -- Main loop
    go level actors
        | S.null actors  = return ()
        | otherwise = do
        cont <- eval level actors
        case cont of
            Nothing      -> return ()
            Just actors' -> go level actors'

    -- Handle actor actions
    eval level = evalActor
        where
        evalActor actors =
            let
                actor   = S.index actors 0
                actors' = S.drop 1 actors
            in case view $ snd actor of
                (NextTick :>>= next) ->
                -- this actor is done, move on to next actor
                    return $ Just $ actors' |>  (fst actor, next ())

                (Return _) -> return Nothing

                (GetRandom range :>>= next) -> do
                    randVal <- randomRIO range
                    evalActor $ (fst actor, next randVal) <| actors'

                (ReadMap :>>= next) ->
                    evalActor $ (fst actor, next level) <| actors'

                (ReadMapWithActors :>>= next) ->
                    evalActor $ (fst actor, next $ addActors actors level) <| actors'

                (GetUserAction :>>= next) -> do
                    action <- getInput
                    evalActor $ (fst actor, next action) <| actors'

                (GetActorPosition :>>= next) ->
                    evalActor $ (fst actor, next $ (actorPos . fst) actor) <| actors'

                (MoveActor new :>>= next) ->
                    let actorData = fst actor
                        nextActor = case addActors actors level ! new of
                            Empty -> actorData { actorPos = new }
                            _     -> actorData
                    in evalActor $ (nextActor, next ()) <| actors'

                (DrawMap :>>= next) -> do
                    draw $ addActors actors level
                    evalActor $ (fst actor, next ()) <| actors'


addActors :: Seq (Actor ()) -> Level -> Level
addActors actors level = let actors' = toList actors in
    level // map ((actorPos &&& ActorBlock . actorImage) . fst) actors'
