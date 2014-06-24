module Actor.Treasure (treasure) where

import Control.Monad
import Graphics.Vty
import Types


treasureImg :: Image
treasureImg = string (withForeColor defAttr brightYellow) "T"


treasure :: Coords -> Actor ()
treasure pos = (ActorData treasureImg pos False, prog)
   where prog = forever nextTick
