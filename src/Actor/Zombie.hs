module Actor.Zombie (zombie) where

import Control.Applicative
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog)
    where
    prog = do
        pos <- getActorPosition
        dyx <- ([(-1, 0), (1, 0), (0, 1), (0, -1)] !!) <$> getRandom (0, 3)
        moveActor $ pos |+| dyx
        nextTick >> prog
