module Bitmap where

import Data.Array.Unboxed
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
