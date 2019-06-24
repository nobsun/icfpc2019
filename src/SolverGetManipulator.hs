{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverGetManipulator
  (solve) where

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
type Path  = SP.Path Point Int Action
-- type Edge  = SP.Edge Point Int Action
-- type ActionPath = [Edge]

solve :: Task -> Solution
solve task = loop Seq.empty s0 bm0 (generateGraph bm0)
  where
    s0 = WW.initialState task
    bm0 = WW.stMap s0

    loop !hist s bm' g'
      | Set.null (WW.stUnwrapped s) = [F.toList hist]
      | otherwise =
          loop
            (hist <> Seq.fromList actions)
            (WW.simulateSolution [actions] s)
            bm
            g
      where
        bm = WW.stMap s
        boosterMap = WW.stBoostersOnMap s
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        -- ドリルで穴を開けられたとき以外は同じものをそのまま使えば良い
        g :: Graph
        g = if bm  == bm' then g' else generateGraph bm

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
          | (p1, cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , let manip = BoosterB `elem` Map.findWithDefault [] p1 boosterMap
          , manip || p1 `Set.member` WW.stUnwrapped s
          ]
