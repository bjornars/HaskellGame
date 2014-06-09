module Actor.Zombie where

import Control.Monad
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie pos = Actor zombieImg pos $ forever noOp
