{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverGetManipulator
  (solve) where

import Data.Function (on)
import Data.List (groupBy, partition)
import Data.Array.IArray
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V

import Task
import qualified ShortestPath as SP
import qualified WorkerWrapper as WW

-- import Control.Monad (join)
-- import Debug.Trace (traceShow)

type Graph = SP.Graph Point Int Action
type Path  = SP.Path Point Int Action
-- type Edge  = SP.Edge Point Int Action
-- type ActionPath = [Edge]

solve :: Task -> Solution
solve task = loop Seq.empty (WW.initialState task)
  where
    loop !hist s
      | Set.null (WW.stUnwrapped s) = [F.toList hist]
      | otherwise =
          loop
            (hist <> Seq.fromList actions)
            (WW.simulateSolution [actions] s)
      where
        bm = WW.stMap s
        boosterMap = WW.stBoostersOnMap s
        bs = bounds bm
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        g :: Graph
        g = HashMap.fromList $ do
              p@(x,y) <- range bs
              return $
                ( p
                , [ (p', 1, act)
                  | (dx,dy,act) <- [(-1,0,MoveLeft), (1,0,MoveRight), (0,-1,MoveDown), (0,1,MoveUp)]
                  , let p' = (x + dx, y + dy)
                  , inRange bs p'
                  , bm ! p'
                  ]
                )

        actions = [ act | (_,_,_,act) <- SP.pathEdges $ head ordered ]

        ordered :: [Path]
        ordered = map snd ms ++ map snd nms
          where (ms, nms) = partition fst minCosts
                -- traceNN xs
                --   | null xs    =  xs
                --   | otherwise  =  join traceShow xs

        minCosts :: [(Bool, Path)]
        minCosts =
          map snd . head $ groupBy ((==) `on` fst)
          [ (cost, (manip, path'))
          | (p1, cost, path') <- SP.dijkstraIncremental SP.path g [p0]
          , p1 /= p0
          , let manip = BoosterB `elem` Map.findWithDefault [] p1 boosterMap
          , manip || p1 `Set.member` WW.stUnwrapped s
          ]
