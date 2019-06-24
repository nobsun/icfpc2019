{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverManipulator
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
import qualified Bitmap

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
      | hasManip  =
          loop
            (hist <> Seq.fromList [attach])
            (WW.simulateSolution [[attach]] s)
            bm
            g
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
        d_arms = WW.wsManipulators w0

        ymin = maybe 0 snd $ Set.lookupMin d_arms
        ymax = maybe 0 snd $ Set.lookupMax d_arms

        attach = AttachManipulator $
          case compare (abs ymin) ymax of
            LT  ->  (1, ymin - 1)
            EQ  ->  (1, ymax + 1)
            GT  ->  (1, ymax + 1)

        manipCount = Map.findWithDefault 0 BoosterB (WW.stBoostersCollected s)
        hasManip = manipCount > 0

        arms = Set.map (\(dx, dy) -> (fst p0 + dx, snd p0 + dy)) d_arms
        visible_ = Bitmap.isVisible bm
        visible = visible_ p0

        visibleArm p1 = p1 `Set.member` arms && visible p1

        -- ドリルで穴を開けられたとき以外は同じものをそのまま使えば良い
        g :: Graph
        g = if bm  == bm' then g' else generateGraph bm

        actions = [ act | (_,_,_,act) <- SP.pathEdges $ head ordered ]

        ordered :: [Path]
        ordered = map (snd . snd) $ ms ++ nva ++ va
          where (ms, nms) = partition fst minCosts
                (nva, va) = partition (fst . snd) nms
                -- traceNN xs
                --   | null xs    =  xs
                --   | otherwise  =  join traceShow xs

        minCosts :: [(Bool, (Bool, Path))]
        minCosts =
          map snd . head $ groupBy ((==) `on` fst)
          [ (cost, (manip, (not $ visibleArm p1, path')))
          | (p1, cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , let manip = BoosterB `elem` Map.findWithDefault [] p1 boosterMap
          , manip || p1 `Set.member` WW.stUnwrapped s
          ]
