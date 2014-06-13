module Actor.Zombie (zombie) where

import Control.Applicative
import Control.Arrow
import Data.Array.IArray
import Data.List
import Data.Ord
import Graphics.Vty
import Types
import Utils


zombieImg :: Image
zombieImg = string (with_fore_color def_attr bright_green) "Z"


awareZombieImg :: Image
awareZombieImg = string (with_fore_color def_attr bright_red) "Z"

data ZombieState = ZombieState {
    timeSinceHasSeen :: Integer
}

zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog (ZombieState 999))
    where
    rememberActorTurns = 7
    prog state = do
        pos   <- getActorPosition
        level <- readMapWithActors
        hero  <- actorPos . head . filter actorIsPlayer <$> getOtherActors
        let canSee = canSeeCoord level pos hero
            timeSinceHasSeen' = if canSee then 0 else timeSinceHasSeen state + 1

        dyx   <- if canSee || timeSinceHasSeen state < rememberActorTurns
                  then do
                      setActorImage awareZombieImg
                      return $ findMove level pos hero
                  else do
                      setActorImage zombieImg
                      (moveDirs !!) <$> getRandom (0, length moveDirs - 1)
        moveActor $ pos |+| dyx
        nextTick >> prog state { timeSinceHasSeen = timeSinceHasSeen' }


moveDirs :: [Coords]
moveDirs = [(-1, 0), (1, 0), (0, 1), (0, -1)]


canSeeCoord :: Level -> Coords -> Coords -> Bool
canSeeCoord level pos target =
    not $ any isWall $ interpolate pos target
    where
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
        distance :: Coords -> Double
        distance = vecLen . toVec . (|-| target)
