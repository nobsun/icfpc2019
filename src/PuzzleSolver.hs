{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PuzzleSolver where

import Control.Monad
import Data.Array.IArray
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Task (Point, Puzzle (..), Task (..))
import ToySolver.SAT as SAT

solve :: Puzzle -> IO ()
solve puzzle@Puzzle{ .. } = do
  solver <- SAT.newSolver

  -- 各セルが、内部(True)か外部(False)かを表す変数
  let cells = [(x,y) | x <- [0..pzTotalSize-1], y <- [0..pzTotalSize-1]]
  (vCells :: Array Point SAT.Var) <- liftM (array ((0,0),(pzTotalSize-1, pzTotalSize-1))) $
    forM cells $ \p -> do
      v <- SAT.newVar solver
      return (p,v)

  forM_ pzIncludes $ \p -> do
    SAT.addClause solver [vCells ! p]
  forM_ pzExcludes $ \p -> do
    SAT.addClause solver [- (vCells ! p)]

  -- 各エッジが、内部と外部の境界(True)か否かを表す変数
  let edges = [((x,y),(x+1,y)) | y <- [0..pzTotalSize], x <- [0..pzTotalSize-1]] ++
              [((x,y),(x,y+1)) | x <- [0..pzTotalSize], y <- [0..pzTotalSize-1]]
  vEdges <- liftM Map.fromList $
    forM edges $ \e -> do
      v <- SAT.newVar solver
      return (e,v)

  -- 各点が頂点かどうかを表す変数
  let points = [(x,y) | x <- [0..pzTotalSize], y <- [0..pzTotalSize]]
  (vCorners :: Array Point SAT.Var) <- liftM (array ((0,0), (pzTotalSize,pzTotalSize))) $
    forM points $ \p -> do
      v <- SAT.newVar solver
      return (p,v)

  -- 隣接するセルについて、境界なら異なるセルの値、境界でなければ同じセルの値
  let f cell1 cell2 edge = do
        SAT.addClause solver [-edge,  cell1,  cell2] -- edge -> ( cell1 ∨  cell2)
        SAT.addClause solver [-edge, -cell1, -cell2] -- edge -> (¬cell1 ∨ ¬cell2)
        SAT.addClause solver [ edge, -cell1,  cell2] -- ¬edge -> cell1 ->  cell2
        SAT.addClause solver [ edge,  cell1, -cell2] -- ¬edge -> cell2 ->  cell1
  -- 横に隣接する場合
  forM_ [0 .. pzTotalSize - 1] $ \y -> do
    forM_ [0 .. pzTotalSize - 2] $ \x -> do
      let cell1 = vCells ! (x, y)
          cell2 = vCells ! (x+1, y)
          edge  = vEdges Map.! ((x+1,y),(x+1,y+1))
      f cell1 cell2 edge
  -- 縦に隣接する場合
  forM_ [0 .. pzTotalSize - 1] $ \x -> do
    forM_ [0 .. pzTotalSize - 2] $ \y -> do
      let cell1 = vCells ! (x, y)
          cell2 = vCells ! (x, y+1)
          edge  = vEdges Map.! ((x,y+1),(x+1,y+1))
      f cell1 cell2 edge

  forM_ points $ \p@(x,y) -> do
    -- 各頂点の次数は 0 または　2
    let es = map (vEdges Map.!) $ 
               [((x-1,y), (x,y)) | 0 <= x-1] ++
               [((x,y), (x+1,y)) | x+1 <= pzTotalSize] ++
               [((x,y-1), (x,y)) | 0 <= y-1] ++
               [((x,y), (x,y+1)) | y+1 <= pzTotalSize]
    vPass <- SAT.newVar solver
    SAT.addPBExactlySoft solver vPass [(1,v) | v<-es] 2
    SAT.addPBAtMostSoft solver (-vPass) [(1,v) | v<-es] 0

    -- 角なら通過
    SAT.addClause solver [- (vCorners ! p), vPass] -- vCorners -> vPass
    -- 角なら横に貫通はしない
    when (0 <= x-1 && x+1 <= pzTotalSize) $
      SAT.addClause solver [- (vCorners ! p), - (vEdges Map.! ((x-1,y),(x,y))), - (vEdges Map.! ((x,y),(x+1,y)))]
    -- 角なら縦に貫通はしない
    when (0 <= y-1 && y+1 <= pzTotalSize) $
      SAT.addClause solver [- (vCorners ! p), - (vEdges Map.! ((x,y-1),(x,y))), - (vEdges Map.! ((x,y),(x,y+1)))]

    -- 門でないなら...
    if (x == 0 || x == pzTotalSize) && (y == 0 || y == pzTotalSize) then do
      -- 角でないなら、通過しない
      SAT.addClause solver [vCorners ! p, - vPass]
    else if x == 0 then
      -- 門でないなら横に延びるエッジは使われない
      SAT.addClause solver [vCorners ! p, - (vEdges Map.! ((x,y), (x+1,y)))]
    else if x == pzTotalSize then
      -- 門でないなら横に延びるエッジは使われない
      SAT.addClause solver [vCorners ! p, - (vEdges Map.! ((x-1,y), (x,y)))]
    else if y == 0 then
      -- 門でないなら縦に延びるエッジは使われない
      SAT.addClause solver [vCorners ! p, - (vEdges Map.! ((x,y), (x,y+1)))]
    else if y == pzTotalSize then
      -- 門でないなら縦に延びるエッジは使われない
      SAT.addClause solver [vCorners ! p, - (vEdges Map.! ((x,y-1), (x,y)))]
    else do
      -- 角でなく、通過する場合には縦に貫通もしくは横に貫通
      v <- SAT.newVar solver
      SAT.addClause solver [vCorners ! p, - vPass, - v, vEdges Map.! ((x,y-1), (x,y))]
      SAT.addClause solver [vCorners ! p, - vPass, - v, vEdges Map.! ((x,y), (x,y+1))]
      SAT.addClause solver [vCorners ! p, - vPass,   v, vEdges Map.! ((x-1,y), (x,y))]
      SAT.addClause solver [vCorners ! p, - vPass,   v, vEdges Map.! ((x,y), (x+1,y))]

  b <- SAT.solve solver  
  print b
  when b $ do
    m <- SAT.getModel solver
    forM_ [pzTotalSize-1, pzTotalSize-2 .. 0] $ \y -> do
      putStrLn [if SAT.evalLit m (vCells ! (x,y)) then '.' else '#' | x <- [0 .. pzTotalSize-1]]

  return ()

{-
(0,2) ---- (1,2) ---- (2,2)
  |          |          |
  |   (0,1)  |   (1,1)  |
  |          |          |
(0,1) ---- (1,1) ---- (2,1)
  |          |          |
  |   (0,0)  |   (1,0)  |
  |          |          |
(0,0) ---- (1,0) ---- (2,0)
-}



test =
  Puzzle
  { pzBlockNumber = 1
  , pzEpochNumber = 1
  , pzTotalSize = 10
  , pzVerticesMin = 4
  , pzVerticesMax = 20
  , pzMNumber = 0
  , pzFNumber = 0
  , pzDNumber = 0
  , pzRNumber = 0
  , pzCNumber = 0
  , pzXNumber = 0
  , pzIncludes = [(0,0)]
  , pzExcludes = [(5,5)]
  }
