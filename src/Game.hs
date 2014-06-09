{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Monad.Operational
import Data.Array.IArray
import Types
import qualified Levels.Level1 as L1

startGame :: (Level -> IO ()) -> IO GAction -> IO ()
startGame draw getInput = uncurry go L1.level
    where
    -- Main loop
    go _ []           = return ()
    go level (actor : xs) = do
        cont <- evalActor level actor
        case cont of
            Nothing               -> return ()
            Just (level', actor') -> go level' (xs ++ [actor'])

    -- Handle actor actions
    evalActor level actor =
        case view $ actorProg actor of
            (Return _) -> return Nothing

            (NoOp :>>= next) ->
                return $ Just (level, actor { actorProg = next () })

            (ReadMap :>>= next) ->
                return $ Just (level, actor { actorProg = next level })

            (GetUserAction :>>= next) -> do
                draw level
                action <- getInput
                return $ Just (level, actor { actorProg = next action })

            (GetActorPosition :>>= next) ->
                return $ Just (level, actor { actorProg = next $ actorPos actor })

            (MoveActor new :>>= next) ->
                let old = actorPos actor
                    img = actorImage actor
                    level' = level // [(old, Empty), (new, ActorBlock img)]
                    nextActor = actor { actorProg = next () }
                in case level ! new of
                    Empty -> return $ Just (level', nextActor { actorPos = new })
                    _     -> return $ Just (level, nextActor)
