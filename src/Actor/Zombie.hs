module Actor.Zombie (zombie) where

import Control.Monad
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = Actor zombieImg initPos $ forever noOp
