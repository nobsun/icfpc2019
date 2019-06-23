{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SolverSimplePrime where

import Data.Array.IArray
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
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
            (hist <> Seq.fromList act')
            (WW.simulateSolution [act'] s)
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
                  | (dx,dy,act) <- [(-1,0,MoveLeft), (1,0,MoveRight), (0,-1,MoveDown), (0,1,MoveUp)]
                  , let p' = (x + dx, y + dy)
                  , inRange bs p'
                  , bm ! p'
                  ]
                )
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
        validActs = maybe [] (\(a,_,_) -> [a]) $ WW.decide s 0 -- V.map snd (WW.validActions s) V.! 0
        act1 :: [Action]
        act1 = take 1 actions
        actions :: [Action]
        actions = head $
          [ [act | (_,_,_,act) <- SP.pathEdges path']
          | (p1, _cost, path') <- SP.dijkstraIncremental SP.path g [p0]
          , p1 /= p0
          , p1 `Set.member` WW.stUnwrapped s
          ]
