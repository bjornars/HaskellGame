module Actor.Zombie (zombie) where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Graphics.Vty
import Pathfinding
import Types
import Utils


zombieImg, awareZombieImg, rememberZombieImg :: Image
zombieImg         = string (with_fore_color def_attr bright_green) "Z"
awareZombieImg    = string (with_fore_color def_attr bright_red) "Z"
rememberZombieImg = string (with_fore_color def_attr red) "Z"

data ZombieState = ZombieState
                 { forgetTimer :: Integer
                 , trackPoint  :: Maybe Coords
                 }

initState :: ZombieState
initState = ZombieState 0 Nothing

zombie :: Coords -> Actor ()
zombie initPos = (ActorData zombieImg initPos False, prog initState)
    where
    rememberActorTurns = 7
    prog state = do
        pos   <- getActorPosition
        level <- readMapWithActors
        pathfindLevel <- readMap
        hero  <- actorPos . head . filter actorIsPlayer <$> getOtherActors

        let canSee = canSeeCoord level pos hero
            timer  = if canSee then rememberActorTurns else forgetTimer state - 1
            target | canSee    = Just hero
                   | timer > 0 = trackPoint state
                   | otherwise = Nothing
            path = target >>= calcPath pathfindLevel pos

        dyx <- case target of
            Just target' -> do
                setActorImage $ if canSee then awareZombieImg else rememberZombieImg
                return $ case path of
                    Just (x:_) -> x |-| pos
                    _          -> (0, 0)
            Nothing -> do
                setActorImage zombieImg
                (moveDirs !!) <$> getRandom (0, length moveDirs - 1)

        moveActor $ pos |+| dyx
        nextTick >> prog state { forgetTimer = timer
                               , trackPoint  = target
                               }


moveDirs :: [Coords]
moveDirs = [(-1, 0), (1, 0), (0, 1), (0, -1)]


canSeeCoord :: Level -> Coords -> Coords -> Bool
canSeeCoord level pos target =
    not $ any isWall $ interpolate pos target
    where
        isWall = (== Wall) . (level !)
