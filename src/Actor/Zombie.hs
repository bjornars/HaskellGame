module Actor.Zombie (zombie) where

import Control.Applicative
import Control.Arrow
import Data.Array.IArray
import Data.List
import Data.Ord
import Graphics.Vty
import Types


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_green) "Z"


awareZombieImg :: Image
awareZombieImg = string (with_fore_color def_attr bright_red) "Z"


zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog)
    where
    prog = do
        pos   <- getActorPosition
        level <- readMapWithActors
        hero  <- actorPos . head . filter actorIsPlayer <$> getOtherActors
        dyx   <- if canSeeCoord level pos hero
                  then do
                      setActorImage awareZombieImg
                      return $ findMove level pos hero
                  else do
                      setActorImage zombieImg
                      (moveDirs !!) <$> getRandom (0, length moveDirs - 1)
        moveActor $ pos |+| dyx
        nextTick >> prog


moveDirs :: [Coords]
moveDirs = [(-1, 0), (1, 0), (0, 1), (0, -1)]


canSeeCoord :: Level -> Coords -> Coords -> Bool
canSeeCoord level pos target =
    let (dy, dx) = target |-| pos
        cellPath = map (pos |+|) [(y, x) | y <- enum dy, x <- enum dx]
    in
        not $ any isWall cellPath
    where
        enum to = if to == 0 then [0] else [0, to `div` abs to .. to]
        isWall = (== Wall) . (level !)


findMove :: Level -> Coords -> Coords -> Coords
findMove level pos target =
    let candidates  = map (id &&& (pos |+|)) moveDirs
        possible    = filter (isEmpty . snd) candidates
        prioritized = map fst $ sortBy (comparing $ distance . snd) possible
    in
        if null prioritized then (0, 0) else head prioritized
    where
        isEmpty  = (== Empty) . (level !)
        distance = vecLen . (|-| target)
