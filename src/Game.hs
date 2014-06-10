{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Monad
import Control.Monad.Operational
import Data.Array.IArray
import System.Random
import Types
import qualified Levels.Level1 as L1

startGame :: (Level -> IO ()) -> IO GAction -> IO ()
startGame draw getInput = uncurry go L1.level []
    where
    -- Main loop
    go _     []           []  = return ()
    go level []           xs  = go level (reverse xs) []
    go level (actor : xs) xs' = do
        when (null xs') $ draw level
        cont <- evalActor level actor
        case cont of
            Nothing               -> return ()
            Just (level', actor') -> go level' xs (actor' : xs')

    -- Handle actor actions
    evalActor level actor =
        case view $ actorProg actor of
            (NextTick :>>= next) ->
            -- this actor is done, move on to next actor
                return $ Just (level, actor { actorProg = next () })

            (Return _) -> return Nothing

            (GetRandom range :>>= next) -> do
                randVal <- randomRIO range
                evalActor level actor { actorProg = next randVal }

            (ReadMap :>>= next) ->
                evalActor level actor { actorProg = next level }

            (GetUserAction :>>= next) -> do
                action <- getInput
                evalActor level actor { actorProg = next action }

            (GetActorPosition :>>= next) ->
                evalActor level actor { actorProg = next $ actorPos actor }

            (MoveActor new :>>= next) ->
                let old = actorPos actor
                    img = actorImage actor
                    level' = level // [(old, Empty), (new, ActorBlock img)]
                    nextActor = actor { actorProg = next () }
                in case level ! new of
                    Empty -> evalActor level' nextActor { actorPos = new }
                    _     -> evalActor level nextActor
