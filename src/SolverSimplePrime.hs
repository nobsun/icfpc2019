{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSimplePrime where

import Data.Array.IArray
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V

import Task
import qualified ShortestPath as SP
import qualified WorkerWrapper as WW


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
        bs = bounds bm
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        g :: SP.Graph Point Int Action
        g = HashMap.fromList $ do
              p@(x,y) <- range bs
              return $
                ( p
                , [ (p', 1, act)
                  | (dx,dy,act) <- [(-1,0,ActionA), (1,0,ActionD), (0,-1,ActionS), (0,1,ActionW)]
                  , let p' = (x + dx, y + dy)
                  , inRange bs p'
                  , bm ! p'
                  ]
                )
        actions :: [Action]
        actions = take 1 $ head $
          [ [act | (_,_,_,act) <- SP.pathEdges path']
          | (p1, _cost, path') <- SP.dijkstraIncremental SP.path g [p0]
          , p1 /= p0
          , p1 `Set.member` WW.stUnwrapped s
          ]
