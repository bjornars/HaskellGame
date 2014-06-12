module Actor.Zombie (zombie) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array.IArray
import Data.List
import Data.Ord
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog)
    where
    prog = do
        pos   <- getActorPosition
        level <- readMapWithActors
        hero  <- liftM (actorPos . head . filter actorIsPlayer) getOtherActors
        dyx   <- if canSeeCoord level hero
                  then return $ findMove level pos hero
                  else (moveDirs !!) <$> getRandom (0, length moveDirs - 1)
        moveActor $ pos |+| dyx
        nextTick >> prog


moveDirs :: [Coords]
moveDirs = [(-1, 0), (1, 0), (0, 1), (0, -1)]


canSeeCoord :: Level -> Coords -> Bool
canSeeCoord _ _ = True


findMove :: Level -> Coords -> Coords -> Coords
findMove level pos hero =
    let candidates  = map (id &&& (pos |+|)) moveDirs
        possible    = filter (isEmpty . snd) candidates
        prioritized = map fst $ sortBy (comparing $ distance . snd) possible
    in
        if null prioritized then (0, 0) else head prioritized
    where
        isEmpty  = (== Empty) . (level !)
        distance = vecLen . (|-| hero)
