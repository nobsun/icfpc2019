{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSimple where

import Data.Array.IArray
import Data.Array.Unboxed
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V

import Task
import qualified ShortestPath as SP
import qualified WorkerWrapper as WW


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
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        -- ドリルで穴を開けられたとき以外は同じものをそのまま使えば良い
        g = if bm  == bm' then g' else generateGraph bm

        actions = head $
          [ [act | (_,_,_,act) <- SP.pathEdges path']
          | (p1, _cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , p1 `Set.member` WW.stUnwrapped s
          ]


generateGraph :: UArray Point Bool -> SP.Graph' Point Int Action
generateGraph bm = Map.fromList $ do
  let bs = bounds bm
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
