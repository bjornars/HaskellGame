{-# LANGUAGE GADTs #-}

module Game (startGame) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Operational
import Data.Array.IArray ((//), (!))
import System.Random (randomRIO)
import Data.Sequence ((|>), (<|), fromList, Seq)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Graphics.Vty

import Types
import qualified Levels.Level1 as L1


drawActor :: Actor ()
drawActor = (ActorData (string (withForeColor defAttr brightRed) " ")  (0, 0) False, prog)
    where prog = drawMap >> nextTick >> prog


startGame :: (Level -> IO ()) -> IO GAction -> IO ()
startGame draw getInput =
    let (level, actors) = L1.level in
    go level $ fromList (drawActor:actors)
    where

    -- Main loop
    go level actors
        | S.null actors = return ()
        | otherwise     = eval level actors >>= go level

    -- Handle actor actions
    eval level actors =
        let
            actor          = S.index actors 0
            actors'        = S.drop 1 actors
            stepActor next = eval level $ (fst actor, next) <| actors'
        in case view $ snd actor of
            (NextTick :>>= next) ->
                -- this actor is done, move on to next actor
                return $ actors' |>  (fst actor, next ())

            (Return _) -> return S.empty

            (GetRandom range :>>= next) ->
                randomRIO range >>= stepActor . next

            (ReadMap :>>= next) ->
                stepActor $ next level

            (ReadMapWithActors :>>= next) ->
                stepActor $ next $ addActors actors level

            (GetUserAction :>>= next) ->
                getInput >>= stepActor . next

            (GetActorPosition :>>= next) ->
                stepActor $ next $ (actorPos . fst) actor

            (GetOtherActors :>>= next) ->
                stepActor $ next $ toList $ fmap fst actors'

            (MoveActor new :>>= next) ->
                let actorData = fst actor
                    nextActor = case addActors actors level ! new of
                        Empty -> actorData { actorPos = new }
                        _     -> actorData
                in eval level $ (nextActor, next ()) <| actors'

            (SetActorImage img :>>= next) ->
                let actor' = (fst actor) { actorImage = img }
                in eval level $ (actor', next ()) <| actors'

            (DrawMap :>>= next) -> do
                draw $ addActors actors level
                stepActor $ next ()

            (KillActor :>>= _) ->
                eval level actors'

            (HurtActor (target, _) :>>= next) ->
                -- ignore amount for now, instakill!
                let nextActors = (fst actor, next()) <| actors'
                    inflict (someActor, prog) = if target == someActor
                        then (someActor, killActor >> prog)
                        else (someActor, prog)
                in
                    eval level $ inflict <$> nextActors


addActors :: Seq (Actor ()) -> Level -> Level
addActors actors level = let actors' = toList actors in
    level // map ((actorPos &&& ActorBlock) . fst) actors'
