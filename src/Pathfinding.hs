{-# LANGUAGE TupleSections #-}

module Pathfinding where

import Control.Monad
import Data.Array ((!))
import Data.PSQueue (Binding(..), minView, keys, insert, singleton, PSQ)
import Data.List (unfoldr)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Types
import Utils


type Cost = Integer
type PriQ = PSQ (Coords, Cost) Cost
type Set  = S.Set Coords
type Path = [Coords]

calcPath :: Level -- the level to pathfind in
         -> Coords -- start
         -> Coords -- destination
         -> Maybe [Coords] -- result list of moves required

-- F = G + H
-- Total = Steps walked + Heuristic

calcPath level start stop 
    | start == stop = Just []
    | otherwise     = go (singleton (start, 0)  (dist start)) S.empty M.empty
    where
        go :: PriQ -> Set -> M.Map Coords Coords -> Maybe [Coords]
        go queue seen path = minView queue >>= go'
          where
            go' ((square, gcost) :-> fcost, rest) =
              if square == stop then return . reverse . (square:) $ makePath path square
              else let
                  seed = (rest, square `S.insert` seen, path)
                  step = processNeighbor (gcost + 1) square
                    in
                  uncurry3 go $ foldr step seed $ options square

        uncurry3 f (a, b, c) = f a b c

        processNeighbor gcost source square (queue, seen, path)
            | square `S.member` seen = (queue, seen, path)  -- todo: check for improvement
            | otherwise             = let fcost = gcost + dist square in
                                        (insert (square, gcost) fcost queue,
                                        square `S.insert` seen,
                                        M.insert square source path)

        -- use manhattan distance, since monsters cannot go diagonally
        dist :: Coords -> Cost
        dist c1 = let absSum (x, y) = abs x + abs y in absSum $ c1 |-| stop

        options :: Coords -> [Coords]
        options = filter ((== Empty) . (level!)) . availables
            where availables square = map (square |+|) [(-1, 0), (1, 0), (0, 1), (0, -1)]

        makePath path = init . unfoldr step
            where step s = (\x -> (x, x)) `fmap` M.lookup s path
