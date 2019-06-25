module Bitmap where

import Data.Array.Unboxed
import qualified Data.Map as Map
import qualified Region
import Task

-- True が通行可能で、 False がマップ外もしくは障害物

buildBitmap :: Task -> UArray Point Bool
buildBitmap task = array ((0,0),(w-1,h-1)) [((x,y), f x y) | x <- [0..w-1], y <- [0..h-1]]
  where
    w = maximum [x | (x,_) <- taskMap task]
    h = maximum [y | (_,y) <- taskMap task]
    boundary = Region.fromList $ taskMap task
    obstacles = map Region.fromList $ taskObstacles task
    f x y = Region.isInside boundary (x,y) && all (\o -> not (Region.isInside o (x,y))) obstacles

bitmapToLines :: UArray Point Bool -> [String]
bitmapToLines bm = do
  let ((xmin,ymin),(xmax,ymax)) = bounds bm
  y <- [ymax,ymax-1..ymin]
  return [if bm ! (x,y) then '.' else '#' | x <- [xmin..xmax]]

printBitmap :: UArray Point Bool -> IO ()
printBitmap = mapM_ putStrLn . bitmapToLines

isVisible :: UArray Point Bool -> Point -> Point -> Bool
isVisible bm from to = all (\p -> inRange (bounds bm) p && (bm ! p)) $ passingCells from to

data Bound = Up | On | Down deriving (Show, Eq, Ord)

-- passingCells' :: Point -> Point -> [Point]
passingCells' p0@(x0,y0) p1@(x1,y1) = filter passed cells
  where
    -- セル位置
    cells = [(floor x, floor y) | x <- [min x0 x1 .. max x0 x1], y <- [min y0 y1 .. max y0 y1]]
    -- 各交点
    corners = [(x, y)| x <- [min x0 x1 .. max x0 x1 + 1], y <- [min y0 y1 .. max y0 y1 + 1]]
    -- 始点終点の座標
    (p0'@(x0',y0'), p1'@(x1',y1')) = ((f x0, f y0), (f x1, f y1)) where f x = x+0.5
    -- 判別式
    judge (x,y)
      | v == 0 = On
      | v >  0 = Up
      | v <  0 = Down
      where v = (y-y0')*(x1'-x0')-(y1'-y0')*(x-x0')
    -- 各交点の判別結果
    judged = Map.fromList $ map (\p@(x,y) -> ((floor x, floor y), judge p)) corners
    -- セルの四隅
    cornerOf (x,y) = [(x',y') | x' <- [x,x+1], y' <- [y,y+1]]
    -- セルを通過するか
    passed p@(x,y) = any (==Up) bs && any (==Down) bs
      where bs = map (judged Map.!) (cornerOf p)

passingCells :: Point -> Point -> [Point]
passingCells p0@(x0,y0) p1@(x1,y1)
  | x0 == x1 = [(x0,y) | y <- [min y0 y1 .. max y0 y1]]
  | y0 == y1 = [(x,y0) | x <- [min x0 x1 .. max x0 x1]]
  | otherwise = concat $
      [ if max l r <= toRational (floor (min l r) + 1)
        then [(x, floor (min l r))]
        else [(x, floor (min l r)), (x, floor (min l r) + 1)]
      | x <- [x0, x0 + signum (x1 - x0) .. x1]
      , let l = f (fromIntegral x)
      , let r = f (fromIntegral x + 1)
      ]
      where
        a :: Rational
        a = fromIntegral (y1 - y0) / fromIntegral (x1 - x0)

        -- (x0 + 0.5, y0 + 0.5) を通る傾き a の直線
        f :: Rational -> Rational
        f x = a * (x - (fromIntegral x0 + 0.5)) + (fromIntegral y0 + 0.5)
