{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Arrow ((&&&))
import Control.Monad.Operational
import Data.Array.IArray hiding (range)
import System.Random (randomRIO)
import Data.Sequence ((|>), (<|), fromList, Seq)
import Data.Foldable (toList, forM_)
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
        forM_ cont $ go level

    -- Handle actor actions
    eval level = evalActor
        where
        evalActor actors =
            let
                actor          = S.index actors 0
                actors'        = S.drop 1 actors
                stepActor next = evalActor $ (fst actor, next) <| actors'
            in case view $ snd actor of
                (NextTick :>>= next) ->
                -- this actor is done, move on to next actor
                    return $ Just $ actors' |>  (fst actor, next ())

                (Return _) -> return Nothing

                (GetRandom range :>>= next) -> do
                    randVal <- randomRIO range
                    stepActor $ next randVal

                (ReadMap :>>= next) ->
                    stepActor $ next level

                (ReadMapWithActors :>>= next) ->
                    stepActor $ next $ addActors actors level

                (GetUserAction :>>= next) -> do
                    action <- getInput
                    stepActor $ next action

                (GetActorPosition :>>= next) ->
                    stepActor $ next $ (actorPos . fst) actor

                (MoveActor new :>>= next) ->
                    let actorData = fst actor
                        nextActor = case addActors actors level ! new of
                            Empty -> actorData { actorPos = new }
                            _     -> actorData
                    in evalActor $ (nextActor, next ()) <| actors'

                (DrawMap :>>= next) -> do
                    draw $ addActors actors level
                    stepActor $ next ()


addActors :: Seq (Actor ()) -> Level -> Level
addActors actors level = let actors' = toList actors in
    level // map ((actorPos &&& ActorBlock) . fst) actors'
