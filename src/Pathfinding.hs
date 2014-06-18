{-# LANGUAGE TupleSections #-}

module Pathfinding where

import Data.Array ((!))
import Data.PSQueue (Binding(..), minView, keys, insert, singleton, PSQ)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Types
import Utils

import Debug.Trace


type Cost = Integer
type PriQ = PSQ (Coords, Cost) Cost
type Set  = S.Set Coords
type Path = [Coords]

calcPath :: Level -- the level to pathfind in
         -> Coords -- start
         -> Coords -- destination
         -> Maybe [Coords] -- result list of hops

-- F = G + H
-- Total = Steps walked + Heuristic

calcPath level start stop = go (singleton (start, 0)  (dist start)) S.empty M.empty
    where
        go :: PriQ -> Set -> M.Map Coords Coords -> Maybe [Coords]
        go queue seen path = case minView queue of
          (Just (m, rest)) -> let ((square, gcost) :-> fcost) = m
                                  in
                              if square == stop then Just $ reverse $ makePath path square
                              else let (queue', seen', path') = foldr (processNeighbor (gcost + 1) square) (rest, square `S.insert` seen, path) (options square) in go queue' seen' path'
          (Nothing) -> Nothing

        -- processNeighbor :: (Coords, Cost) -> (PriQ, Set) -> (PriQ, Set)
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
        options square = availables
          where
              possibles = map (square |+|) [(-1, 0), (1, 0), (0, 1), (0, -1)]
              availables :: [Coords]
              availables = filter ((== Empty) . (level!)) possibles

        makePath path = mk
            where
              mk square = case M.lookup square path of
                (Just src) -> src : mk src
                (Nothing) -> []
