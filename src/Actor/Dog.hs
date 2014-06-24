module Actor.Dog (dog) where

import Control.Applicative
import Control.Arrow
import Data.Array.IArray
import Data.List
import Data.Ord
import Graphics.Vty
import Pathfinding
import Types
import Utils


img :: Image
img = string (withForeColor defAttr brightBlue) "D"


dog :: Coords -> Actor ()
dog initPos = (ActorData img initPos False, prog )
    where
    prog = do
        pos   <- getActorPosition
        level <- readMap
        hero  <- actorPos . head . filter actorIsPlayer <$> getOtherActors
        case calcPath level pos hero of
             (Just (x:_)) -> moveActor x
             _           -> return ()

        nextTick >> prog
