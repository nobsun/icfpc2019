module WorkerWrapper where

import Control.Monad (forM_)
import Data.Array.Unboxed

import Task hiding (TaskMap, taskMap, taskPoint, taskObstacles, taskBoosters)
import Bitmap (buildBitmap)

type Velocity = Int

type Cell = Char

data WorkerWrapper = WW
  { taskMap       :: UArray Point Bool
  , taskPoint     :: Point
  , taskBoosters  :: [(BoosterCode, Point)]

  , wwPosition    :: Point
  , wwArms        :: [Point]
  , wwBoosters    :: [(BoosterCode, Int)]
  , wwFrontier    :: UArray Point Bool
  , wwSpeed       :: Velocity
  } deriving (Show, Eq, Ord)

initWW :: Task -> WorkerWrapper
initWW t@(Task m p o b) =
  WW { taskMap = bm
     , taskPoint = p
     , taskBoosters = b

     , wwPosition = p
     , wwArms = [(0,0),(1,0),(1,1),(1,-1)]
     , wwBoosters = []
     , wwFrontier = bm
     , wwSpeed = 1
     }
  where
    bm = buildBitmap t

drawMap prob = do
  forM_ [h,h-1..0] $ \y -> do
    forM_ [0..w] $ \x -> do
      putChar $ drawCell ww (x, y)
    putChar '\n'
  where
    Right t = parseTask prob
    ww = initWW t
    (_, (w, h)) = bounds $ taskMap ww

drawCell :: WorkerWrapper -> Point -> Char
drawCell ww pos@(x, y) =
  if wwPosition ww == pos
  then '@'
  else if wwFrontier ww ! pos
  then '.'
  else if taskMap ww ! pos
  then '='
  else '#'
