module Bitmap where

import Control.Arrow ((&&&),(***))
import Data.Array.Unboxed
import qualified Data.Map as Map
import Data.Ratio
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

passingCells :: Point -> Point -> [Point]
passingCells p0@(x0,y0) p1@(x1,y1)
  | p0 == p1  = [p0]
  | otherwise = filter passed cells
  where
    (minX, maxX, minY, maxY) = (min x0 x1, max x0 x1, min y0 y1, max y0 y1)
    -- セル位置
    cells :: [Point]
    cells = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
    -- 各交点
    corners :: [Point]
    corners = [(x, y)| x <- [minX .. maxX+1], y <- [minY .. maxY+1]]
    -- 始点終点の座標
    (p0'@(x0',y0'), p1'@(x1',y1')) = ((f x0, f y0), (f x1, f y1)) where f x = fromIntegral x + 1%2
    -- 判別式
    judge :: Point -> Bound
    judge (x,y)
      | v == 0 = On
      | v >  0 = Up
      | v <  0 = Down
      where v =  (x1'-x0')*(fromIntegral y-y0')-(y1'-y0')*(fromIntegral x-x0')
    -- 各交点の判別結果
    judged :: Map.Map Point Bound
    judged = Map.fromList $ map (id &&& judge) corners
    -- セルの四隅
    cornerOf :: Point -> [Point]
    cornerOf (x,y) = [(x',y') | x' <- [x,x+1], y' <- [y,y+1]]
    -- セルを通過するか
    passed :: Point -> Bool
    passed p@(x,y) = Up `elem` bs && Down `elem` bs
      where bs = map (judged Map.!) (cornerOf p)
