module Bitmap where

import Data.Ratio ((%))
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

isVisible :: UArray Point Bool -> Point -> Point -> Bool
isVisible bm from to = all (\p -> inRange (bounds bm) p && (bm ! p)) $ passingCells from to

passingCells_ :: Point -> Point -> [Point]
passingCells_ p0@(x0,y0) p1@(x1,y1)
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

-----

passingCells :: Point -> Point -> [Point]
passingCells p0@(x0,y0) p1@(x1,y1)
  | p0 == p1        = [p0]
  | abs a <= 1 % 1  =
    if x0 > x1
    then passingCellsX p1 p0
    else passingCellsX p0 p1
  | otherwise       =
    if y0 > y1
    then passingCellsY p1 p0
    else passingCellsY p0 p1
  where
    a :: Rational
    a = fromIntegral (y1 - y0) / fromIntegral (x1 - x0)

passingCellsX :: Point -> Point -> [Point]
passingCellsX (x0,y0) (x1,y1)
  | a > 0  =  recP x0 y0 $ takeWhile (< ey) dysI
  | a < 0  =  recN x0 y0 $ takeWhile (> ey) dysI
  | otherwise = [(x, y0) | x <- [x0 .. x1] ]
  where
    a :: Rational
    a = fromIntegral (y1 - y0) / fromIntegral (x1 - x0)

    sy = fromIntegral y0 + 1 % 2
    ey = fromIntegral y1 + 1 % 2

    dysI = [ sy + (1 % 2) * a, sy + (1 % 2) * a + a .. ]

    {- a <= 1 なので dy <= y + 2 -}
    recP :: Int -> Int -> [Rational] -> [Point]
    recP x y []                   = [(x, y)]
    recP x y (dy : dys)
      | dy <  ry1                 =  (x, y) : recP (x + 1) y dys
      | dy >  ry1                 =  (x, y) : (x, y + 1) : recP (x + 1) (y + 1) dys
      | otherwise {- dy == ry1 -} =  (x, y) : recP (x + 1) (y + 1) dys
        where ry1 = fromIntegral $ y + 1

    {- a >= -1 なので dy >= y - 1 -}
    recN x y []                   = [(x, y)]
    recN x y (dy : dys)
      | dy >  ry                  =  (x, y) : recN (x + 1) y dys
      | dy <  ry                  =  (x, y) : (x, y - 1) : recN (x + 1) (y - 1) dys
      | otherwise {- dy == ry  -} =  (x, y) : recN (x + 1) (y - 1) dys
        where ry  = fromIntegral y

passingCellsY :: Point -> Point -> [Point]
passingCellsY (x0,y0) (x1,y1)
  | b > 0  =  recP x0 y0 $ takeWhile (< ex) dxsI
  | b < 0  =  recN x0 y0 $ takeWhile (> ex) dxsI
  | otherwise = [(x0, y) | y <- [y0 .. y1] ]
  where
    b :: Rational
    b = fromIntegral (x1 - x0) / fromIntegral (y1 - y0)

    sx = fromIntegral x0 + 1 % 2
    ex = fromIntegral x1 + 1 % 2

    dxsI = [ sx + (1 % 2) * b, sx + (1 % 2) * b + b .. ]

    {- b <= 1 なので dx <= x + 2 -}
    recP x y []                   = [(x, y)]
    recP x y (dx : dxs)
      | dx <  rx1                 =  (x, y) : recP x (y + 1) dxs
      | dx >  rx1                 =  (x, y) : (x + 1, y) : recP (x + 1) (y + 1) dxs
      | otherwise {- dx == rx1 -} =  (x, y) : recP (x + 1) (y + 1) dxs
        where rx1 = fromIntegral $ x + 1

    {- b >= -1 なので dx >= x - 1 -}
    recN x y []                   = [(x, y)]
    recN x y (dx : dxs)
      | dx >  rx                  =  (x, y) : recN x (y + 1) dxs
      | dx <  rx                  =  (x, y) : (x - 1, y) : recN (x - 1) (y + 1) dxs
      | otherwise {- dx == rx  -} =  (x, y) : recN (x - 1) (y + 1) dxs
        where rx  = fromIntegral x
