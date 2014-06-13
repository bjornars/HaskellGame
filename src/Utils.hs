module Utils where

import Control.Arrow
import Control.Monad
import Types


(|-|), (|+|), (|*|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |-| (x2, y2) = (x1 - x2, y1 - y2)
(x1, y1) |+| (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) |*| (x2, y2) = (x1 * x2, y1 * y2)


both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)


toVec :: Floating a => Coords -> (a, a)
toVec = both fromInteger


vecLen :: Floating a => (a, a) -> a
vecLen (x, y) = sqrt $ x^(2 :: Int) + y^(2 :: Int)
