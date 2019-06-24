{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PuzzleSolver where

import Control.Monad
import Data.Array.IArray
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Task (Point, Puzzle (..), Task (..))
import qualified ToySolver.SAT as SAT

solve :: Puzzle -> IO ()
solve puzzle@Puzzle{ .. } = do
  solver <- SAT.newSolver

  -- 4. M should be contained within the non-negative square with x/y-coordinates less or equal tSize
  -- (the task is not too large).

  -- 各セルが、内部(True)か外部(False)かを表す変数
  let cells = [(x,y) | x <- [0..pzTotalSize-1], y <- [0..pzTotalSize-1]]
  (vCells :: Array Point SAT.Var) <- liftM (array ((0,0),(pzTotalSize-1, pzTotalSize-1))) $
    forM cells $ \p -> do
      v <- SAT.newVar solver
      return (p,v)

  -- 9. M should contain all squares with coordinates from iSqs.
  forM_ pzIncludes $ \p -> do
    SAT.addClause solver [vCells ! p]
  -- 10. M must contain no squares with coordinates from oSqs.
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

  -- 外部と隣接する場合、境界なら内部、境界でないなら外部
  let g cell edge = do
        SAT.addClause solver [-edge, cell] --  edge -> cell
        SAT.addClause solver [edge, -cell] -- ¬edge -> ¬cell
  -- 横に隣接する場合
  forM_ [0 .. pzTotalSize - 1] $ \y -> do
    g (vCells ! (0, y)) (vEdges Map.! ((0,y),(0,y+1)))
    g (vCells ! (pzTotalSize-1, y)) (vEdges Map.! ((pzTotalSize,y),(pzTotalSize,y+1)))
  -- 縦に隣接する場合
  forM_ [0 .. pzTotalSize - 1] $ \x -> do
    g (vCells ! (x, 0)) (vEdges Map.! ((x,0),(x+1,0)))
    g (vCells ! (x, pzTotalSize-1)) (vEdges Map.! ((x,pzTotalSize),(x+1,pzTotalSize)))

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

  -- 5. At least one of the maximal x/y-dimensions of M should be larger or equal than
  -- tSize − ⌊0.1 × tSize⌋ (the task is not too small).
  let tmp = pzTotalSize - floor (0.1 * toRational pzTotalSize)
  SAT.addClause solver [v | ((x,y),v) <- assocs vCells, tmp <= x+1 || tmp <= y+1]

  -- 6. The area of M must be at least ⌈0.2 × tSize**2⌉ (the task is not too sparse).
  SAT.addAtLeast solver (elems vCells) (ceiling ((toRational pzTotalSize ^ (2 ::Int)) * 0.5))

  -- 7. The polygon M should have no less than vMin and no more than vMax vertices
  -- (the task is not too boring and not too hairy).
  SAT.addAtLeast solver (elems vCorners) pzVerticesMin
  SAT.addAtMost solver (elems vCorners) pzVerticesMax

  -- 2. The puzzle-solving task may have no obstacles (unlike the tasks from the main contest).
  -- 3. The initial position of the worker-wrapper should be within the map M.

  -- 8. The task should contain precisely the numbers of boosters specified by the puzzle:
  -- • mNum manipulator extensions
  -- • fNum fast wheels,
  -- • dNum drills,
  -- • rNum teleports,
  -- • cNum cloning boosters,
  -- • xNum spawn points.

  b <- SAT.solve solver  
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


-- chain-puzzle-examples/puzzle.cond
puzzle =
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

test = do
  print puzzle
  solve puzzle
