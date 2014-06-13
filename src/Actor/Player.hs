module Actor.Player (player) where

import Control.Applicative
import Graphics.Vty
import Types


playerImg :: Image
playerImg = string (with_fore_color def_attr bright_blue) "@"


player :: Coords -> Actor ()
player initPos = (ActorData playerImg initPos True, prog)
    where
    prog = do
        action <- getUserAction
        case action of
            Quit     -> return ()
            Move dir -> do
                pos <- getActorPosition
                moveActor $ pos |+| dyx dir
                nextTick >> prog
            Attack -> do
                zombie  <- head . filter (not . actorIsPlayer) <$> getOtherActors
                hurtActor (zombie, 10)
                nextTick >> prog
            _ -> prog
    dyx DUp    = (-1,  0)
    dyx DDown  = ( 1,  0)
    dyx DLeft  = ( 0, -1)
    dyx DRight = ( 0,  1)
