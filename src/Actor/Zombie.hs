module Actor.Zombie (zombie) where

import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = Actor zombieImg initPos prog
    where
    prog = do
        pos <- getActorPosition
        direction <- getRandom (0, 3)
        let dyx = [(-1, 0), (1, 0), (0, 1), (0, -1)] !! direction
        moveActor $ pos |+| dyx
        nextTick >> prog
