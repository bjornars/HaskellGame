{-# LANGUAGE GADTs #-}

module Types where

import Control.Monad.Operational
import Data.Array.IArray
import Graphics.Vty

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
    NextTick :: ActorOp ()
    GetActorPosition :: ActorOp Coords
    GetUserAction :: ActorOp GAction
    MoveActor :: Coords -> ActorOp ()
    ReadMap :: ActorOp Level


data Actor a = Actor
             { actorImage :: Image
             , actorPos :: Coords
             , actorProg :: Program ActorOp a
             }


type ActorP a = Program ActorOp a


nextTick :: ActorP ()
nextTick = singleton NextTick


getActorPosition :: ActorP Coords
getActorPosition = singleton GetActorPosition


getUserAction :: ActorP GAction
getUserAction = singleton GetUserAction


moveActor :: Coords -> ActorP ()
moveActor = singleton . MoveActor


readMap :: ActorP Level
readMap = singleton ReadMap


data Block = Wall
           | Empty
           | Void
           | ActorBlock Image
           deriving (Eq, Show)

(|-|), (|+|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)
