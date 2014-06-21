module Utils where

import Control.Arrow
import Control.Monad
import Types

-- lift a binary operator to work pairwise on tuples
liftVec :: Num a => (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
liftVec f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

(|-|), (|+|), (|*|) :: Num a => (a, a) -> (a, a) -> (a, a)
(|-|) = liftVec (-)
(|+|) = liftVec (+)
(|*|) = liftVec (*)


both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)


-- | Interpolate between two coordinates.
--
-- This returns an inclusive list of the coordinates between start and
-- stop.
--
-- > interpolate (4, 6) (7, 8)
-- > [(4,6),(5,7),(6,7),(7,8)]
--
interpolate :: Coords -> Coords -> [Coords]
interpolate start stop = reverse $ go (0, 0) [start]
    where
        dyx  :: (Double, Double)
        dyx  = toVec $ stop |-| start
        max' = uncurry max $ both abs dyx
        step = both (/ max') dyx
        go current coords =
            let next = current |+| step
            in if vecLen next <= vecLen dyx
                   then go next ((start |+| both round next) : coords)
                   else coords


toVec :: Floating a => Coords -> (a, a)
toVec = both fromInteger


vecLen :: Floating a => (a, a) -> a
vecLen (x, y) = sqrt $ x^(2 :: Int) + y^(2 :: Int)
