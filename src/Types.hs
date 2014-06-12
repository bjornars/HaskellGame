{-# LANGUAGE GADTs #-}

module Types where

import Control.Monad.Operational
import Data.Array.IArray
import Graphics.Vty
import System.Random

-- | Prefix directions with 'D' to not conflict with Either (Left, Right)
data Direction = DUp | DDown | DLeft | DRight
               deriving (Eq)

-- | Game Action
data GAction
    = Quit
    | Attack
    | Move Direction
    | ChangeLevel
    | None
    deriving (Eq)

type Coords = (Integer, Integer)
type LevelArray a = Array Coords a
type Level = LevelArray Block


{- GADT of all actor operations. Use the singleton functions
  for executing operations within the actor programs. -}
data ActorOp a where
    GetRandom :: Random a => (a, a) -> ActorOp a
    NextTick :: ActorOp ()
    GetOtherActors :: ActorOp [ActorData]
    GetActorPosition :: ActorOp Coords
    GetUserAction :: ActorOp GAction
    MoveActor :: Coords -> ActorOp ()
    DrawMap :: ActorOp ()
    ReadMap :: ActorOp Level
    ReadMapWithActors :: ActorOp Level


data ActorData = ActorData
               { actorImage :: Image
               , actorPos :: Coords
               , actorIsPlayer :: Bool
               } deriving (Eq, Show)


type Actor  a = (ActorData, Program ActorOp a)
type ActorP a = Program ActorOp a


getRandom :: Random a => (a, a) -> ActorP a
getRandom = singleton . GetRandom


nextTick :: ActorP ()
nextTick = singleton NextTick


drawMap :: ActorP ()
drawMap = singleton DrawMap


getActorPosition :: ActorP Coords
getActorPosition = singleton GetActorPosition


getOtherActors :: ActorP [ActorData]
getOtherActors = singleton GetOtherActors


getUserAction :: ActorP GAction
getUserAction = singleton GetUserAction


moveActor :: Coords -> ActorP ()
moveActor = singleton . MoveActor


readMap :: ActorP Level
readMap = singleton ReadMap


readMapWithActors :: ActorP Level
readMapWithActors = singleton ReadMapWithActors


drawActor :: Actor ()
drawActor = (ActorData (string (with_fore_color def_attr bright_red) " ")  (0, 0) False, prog)
    where prog = drawMap >> nextTick >> prog


data Block = Wall
           | Empty
           | Void
           | ActorBlock ActorData
           deriving (Eq, Show)

(|-|), (|+|), (|*|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) |*| (x2, y2) = (x1 * x2, y1 * y2)
