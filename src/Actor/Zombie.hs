module Actor.Zombie (zombie) where

import Control.Applicative
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = Actor zombieImg initPos prog
    where
    prog = do
        pos <- getActorPosition
        dyx <- (,) <$> getRandom (-1, 1) <*> getRandom (-1, 1)
        moveActor $ pos |+| dyx
        nextTick >> prog
