module Actor.Zombie (Zombie, mkZombie) where

import Graphics.Vty
import Types


data Zombie = Zombie
            { zHealth :: Int
            , zPos :: Coords
            , zState :: MonsterState
            } deriving (Show)


instance Monster Zombie where
    mImage = undefined
    mHurt z = z
    mTick z = z
    mState = zState
    mPos = zPos


mkZombie :: Coords -> Zombie
mkZombie p = Zombie 5 p Living
