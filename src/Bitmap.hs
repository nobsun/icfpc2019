module Bitmap where

import Control.Applicative
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

{-
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
 -}

-----

data Region = Neg | Zero | Pos deriving (Show, Eq, Ord)

passingCells :: Point -> Point -> [Point]
passingCells p0@(x0, y0) (x1, y1) = go p0
  where
    (sx, sy) = (fromIntegral x0 + 1/2, fromIntegral y0 + 1/2) {- 始点 -}
    (ex, ey) = (fromIntegral x1 + 1/2, fromIntegral y1 + 1/2) {- 終点 -}
    (segx, segy) = (ex - sx, ey - sy) {- 線分の変位 -}

    {- 領域判別 -}
    discriminant :: Point -> Region
    discriminant (x, y)
      | v < 0      =  Neg
      | v > 0      =  Pos
      | otherwise  =  Zero
      where
        v = segy * (fromIntegral x - ex) - segx * (fromIntegral y - ey)

    {- セルの中心点 -}
    centerOf (x, y) = (fromIntegral x + 1/2, fromIntegral y + 1/2)

    {- セル中心点の終点までの内積距離 -}
    enorm :: Point -> Rational
    enorm p = abs (segx * (x - ex) + segy * (y - ey))  where (x, y) = centerOf p

    {- セルを通るか -}
    passing :: Point -> Bool
    passing (x, y) = Neg `elem` ds && Pos `elem` ds
      where ds = map discriminant $ (,) <$> [x, x + 1] <*> [y, y + 1]

    go :: Point -> [Point]
    go p@(x, y)
      | enorm p == 0  =  [p]
      | otherwise     =  p : go (head nexts)
      where
        nexts =
          [ q
          | (a, b) <- [ (1, 0), (0, 1), (-1, 0), (0, -1) ] <|> {- 隣りを優先 -}
                      (,) <$> [1, -1] <*> [1, -1]              {- 隣りが無ければ斜め -}
          , let q = (x + a, y + b)
          , enorm q < enorm p && passing q {- 終点に近づく かつ セルを通る -}
          ]

-----

passingCells_inclination :: Point -> Point -> [Point]
passingCells_inclination p0@(x0,y0) p1@(x1,y1)
  | p0 == p1        = [p0]
  | abs a <= 1      =
    if x0 > x1
    then passingX p1 p0
    else passingX p0 p1
  | otherwise       =
    if y0 > y1
    then passingY p1 p0
    else passingY p0 p1
  where
    a :: Rational
    a = fromIntegral (y1 - y0) / fromIntegral (x1 - x0)

passingX :: Point -> Point -> [Point]
passingX (x0,y0) (x1,y1)
  | a == 0     =  [(x, y0) | x <- [x0 .. x1] ]
  | otherwise  =  passing x0 y0 dys0
  where
    a :: Rational
    a = fromIntegral (y1 - y0) / fromIntegral (x1 - x0)

    sy, ey :: Rational
    sy = fromIntegral y0
    ey = fromIntegral y1

    ((|+|), (/+/), (/</), takeP)
      | a > 0  =  ((+), (+), (<), (< ey))
      | a < 0  =  ((-), (-), (>), (> ey))

    iv = sy + (1 / 2) * a
    dys0 = takeWhile takeP [ iv, iv + a .. ]

    passing :: Int -> Int -> [Rational] -> [Point]
    passing = rec_
      where
        {-- |a| <= 1 なので | dy - y | <= 3/2 --}
        rec_ x y []                   = [(x, y)]
        rec_ x y (dy : dys)
          | dy /</ ry                 =  (x, y) : rec_ (x + 1) y dys
          | dy == ry                  =  (x, y) : rec_ (x + 1) (y |+| 1) dys
          | otherwise                 =  (x, y) : (x, y |+| 1) : rec_ (x + 1) (y |+| 1) dys
          where ry  = fromIntegral y /+/ (1 / 2)

passingY :: Point -> Point -> [Point]
passingY (x0,y0) (x1,y1)
  | b == 0     =  [(x0, y) | y <- [y0 .. y1] ]
  | otherwise  =  passing x0 y0 dxs0
  where
    b :: Rational
    b = fromIntegral (x1 - x0) / fromIntegral (y1 - y0)

    sx = fromIntegral x0
    ex = fromIntegral x1

    ((|+|), (/+/), (/</), takeP)
      | b > 0  =  ((+), (+), (<), (< ex))
      | b < 0  =  ((-), (-), (>), (> ex))

    iv = sx + (1 / 2) * b
    dxs0 = takeWhile takeP [ iv, iv + b .. ]

    passing :: Int -> Int -> [Rational] -> [Point]
    passing = rec_
      where
        rec_ x y []                   = [(x, y)]
        rec_ x y (dx : dxs)
          | dx /</ rx                 =  (x, y) : rec_ x (y + 1) dxs
          | dx == rx                  =  (x, y) : rec_ (x |+| 1) (y + 1) dxs
          | otherwise                 =  (x, y) : (x |+| 1, y) : rec_ (x |+| 1) (y + 1) dxs
          where rx  = fromIntegral x /+/ (1 / 2)
