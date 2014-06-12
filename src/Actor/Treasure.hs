module Actor.Treasure (treasure) where

import Control.Monad
import Graphics.Vty
import Types


treasureImg :: Image
treasureImg = string (with_fore_color def_attr bright_yellow) "T"


treasure :: Coords -> Actor ()
treasure pos = (ActorData treasureImg pos False, prog)
   where prog = forever nextTick
