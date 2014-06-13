module Actor.Player (player) where

import Control.Applicative
import Control.Monad
import Graphics.Vty
import Types

import Data.List
import Data.Ord

playerImg :: Image
playerImg = string (with_fore_color def_attr bright_blue) "@"


player :: Coords -> Actor ()
player initPos = (ActorData playerImg initPos True, prog)
    where
    distance target = vecLen . (|-| target)
    prog = do
        action <- getUserAction
        case action of
            Quit     -> return ()
            Move dir -> do
                pos <- getActorPosition
                moveActor $ pos |+| dyx dir
                nextTick >> prog
            Attack -> do
                pos <- getActorPosition
                zombies  <- filter (not . actorIsPlayer) <$> getOtherActors
                -- zombie or treasure, is more like it.. or DrawActor, which is a tad unfortunate
                unless (null zombies) $
                    let zombie = minimumBy (comparing $ distance pos . actorPos) zombies in
                    hurtActor (zombie, 10)
                nextTick >> prog
            _ -> prog
    dyx DUp    = (-1,  0)
    dyx DDown  = ( 1,  0)
    dyx DLeft  = ( 0, -1)
    dyx DRight = ( 0,  1)
