module Actor.Zombie (zombie) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog)
    where
    prog = do
        pos   <- getActorPosition
        level <- readMap
        hero  <- liftM (head . filter actorIsPlayer) getOtherActors
        dyx   <- if canSeeCoord level (actorPos hero)
                  then return $ moveTowards pos (actorPos hero)
                  else ([(-1, 0), (1, 0), (0, 1), (0, -1)] !!) <$> getRandom (0, 3)
        moveActor $ pos |+| dyx
        nextTick >> prog


canSeeCoord :: Level -> Coords -> Bool
canSeeCoord _ _ = True


moveTowards :: Coords -> Coords -> Coords
moveTowards pos hero =
    let vector = pos |-| hero
        both   = join (***)
        denom  = uncurry max $ both abs vector
    in (-1, -1) |*| both (`div` denom) vector
