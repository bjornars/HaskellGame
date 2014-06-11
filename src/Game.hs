{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Arrow ((&&&))
import Control.Monad.Operational
import Data.Array.IArray hiding (range)
import System.Random (randomRIO)
import Types
import qualified Levels.Level1 as L1

startGame :: (Level -> IO ()) -> IO GAction -> IO ()
startGame draw getInput =
    let (level, actors) = L1.level in
    go level (drawActor:actors)
    where
    -- Main loop
    go _     [] = return ()
    go level (actor : xs) = do
        let actors = actor : xs
        cont <- eval actors level actor
        case cont of
            Nothing               -> return ()
            Just (level', actor') -> go level' $ xs ++ [actor']

    -- Handle actor actions
    eval actors level = evalActor
        where
        evalActor actor = case view $ actorProg actor of
            (NextTick :>>= next) ->
            -- this actor is done, move on to next actor
                return $ Just (level, actor { actorProg = next () })

            (Return _) -> return Nothing

            (GetRandom range :>>= next) -> do
                randVal <- randomRIO range
                evalActor actor { actorProg = next randVal }

            (ReadMap :>>= next) ->
                evalActor actor { actorProg = next level }

            (ReadMapWithActors :>>= next) ->
                evalActor actor { actorProg = next $ addActors actors level }

            (GetUserAction :>>= next) -> do
                action <- getInput
                evalActor actor { actorProg = next action }

            (GetActorPosition :>>= next) ->
                evalActor actor { actorProg = next $ actorPos actor }

            (MoveActor new :>>= next) ->
                let nextActor = actor { actorProg = next () }
                in case addActors actors level ! new of
                    Empty -> evalActor nextActor { actorPos = new }
                    _     -> evalActor nextActor

            (DrawMap :>>= next) -> do
                draw $ addActors actors level
                evalActor actor { actorProg = next () }


addActors :: [Actor ()] -> Level -> Level
addActors actors level = level // map (actorPos &&& ActorBlock . actorImage) actors
