{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverGetManipulator where

import Data.Function (on)
import Data.List (groupBy, partition)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V

import SolverSimple (generateGraph)
import Task
import qualified ShortestPath as SP
import qualified WorkerWrapper as WW

-- import Control.Monad (join)
-- import Debug.Trace (traceShow)

type Graph = SP.Graph' Point Int Action
type Edge  = SP.Edge Point Int Action
type ActionPath = [Edge]

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
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        g :: Graph
        g = generateGraph bm

        actions = [ act | (_,_,_,act) <- head ordered ]

        ordered :: [ActionPath]
        ordered = map snd ms ++ map snd nms
          where (ms, nms) = partition fst minCosts
                -- traceNN xs
                --   | null xs    =  xs
                --   | otherwise  =  join traceShow xs

        minCosts :: [(Bool, ActionPath)]
        minCosts =
          map snd . head $ groupBy ((==) `on` fst)
          [ (cost, (manip, es))
          | (p1, cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , let manip = BoosterB `elem` Map.findWithDefault [] p1 boosterMap
          , manip || p1 `Set.member` WW.stUnwrapped s
          , let es = SP.pathEdges path'
          ]
