{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSimplePrime where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V

import SolverSimple (generateGraph)
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
            (hist <> Seq.fromList act')
            (WW.simulateSolution [act'] s)
            bm
            g
      where
        bm = WW.stMap s
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        -- ドリルで穴を開けられたとき以外は同じものをそのまま使えば良い
        g :: SP.Graph' Point Int Action
        g = if bm  == bm' then g' else generateGraph bm

        isActionB (AttachManipulator _) = True
        isActionB _ = False
        isActionEQ TurnCW = True
        isActionEQ TurnCCW = True
        isActionEQ _ = False
        act' :: [Action]
        act' = if null bs && null eqs
               then act1
               else if null bs then head eqs:act1
                    else head bs:act1
          where bs  = filter isActionB validActs
                eqs = filter isActionEQ validActs
        validActs :: [Action]
        validActs = maybe [] (\(a,_,e) -> if e < Set.size (WW.stUnwrapped s) then [a] else []) $ WW.decide s 0 -- V.map snd (WW.validActions s) V.! 0
        act1 :: [Action]
        act1 = take 1 actions
        actions :: [Action]
        actions = head $
          [ [act | (_,_,_,act) <- SP.pathEdges path']
          | (p1, _cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , p1 `Set.member` WW.stUnwrapped s
          ]
