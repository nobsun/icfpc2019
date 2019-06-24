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
solve task = loop Seq.empty (WW.initialState task)
  where
    loop !hist s
      | Set.null (WW.stUnwrapped s) = [F.toList hist]
      | otherwise =
          loop
            (hist <> Seq.fromList act')
            (WW.simulateSolution [act'] s)
      where
        bm = WW.stMap s
        w0 = (WW.stWrappers s) V.! 0
        p0 = WW.wsPosition w0

        g :: SP.Graph' Point Int Action
        g = generateGraph bm

        isActionB (AttachManipulator _) = True
        isActionB _ = False
        isActionEQ TurnCW = True
        isActionEQ TurnCCW = True
        isActionEQ _ = False
        act' :: [Action]
        act' = if null bs then act1 else head bs:act1
          where bs  = filter isActionB validActs
                eqs = filter isActionEQ validActs
        validActs :: [Action]
        validActs = V.map snd (WW.validActions s) V.! 0
        act1 :: [Action]
        act1 = take 1 actions
        actions :: [Action]
        actions = head $
          [ [act | (_,_,_,act) <- SP.pathEdges path']
          | (p1, _cost, path') <- SP.dijkstraIncremental' SP.path g [p0]
          , p1 /= p0
          , p1 `Set.member` WW.stUnwrapped s
          ]
