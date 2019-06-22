module WorkerWrapper where

import Control.Monad (forM_)
import Data.Set as Set
import qualified Data.Array.Unboxed as UA

import Task hiding (TaskMap, taskMap, taskPoint, taskObstacles, taskBoosters)
import Bitmap (buildBitmap)

type Velocity = Int
type TaskMap = Set Point

data WorkerWrapper = WW
  { taskMap       :: TaskMap
  , taskPoint     :: Point
  , taskObstacles :: TaskMap
  , taskBoosters  :: [(BoosterCode, Point)]

  , wwPosition    :: Point
  , wwArms        :: TaskMap
  , wwBoosters    :: [(BoosterCode, Int)]
  , wwFrontier    :: TaskMap
  , wwSpeed       :: Velocity
  } deriving (Show, Eq, Ord)

initWW :: Task -> WorkerWrapper
initWW t@(Task m p o b) =
  WW { taskMap = fs
     , taskPoint = p
     , taskObstacles = os
     , taskBoosters = b
     
     , wwArms = fromList [(0,0),(1,0),(1,1),(1,-1)]
     , wwBoosters = []
     , wwFrontier = fs
     , wwSpeed = 1
     }
  where
    pair (f, g) x = (f x, g x)
    tupply f (x, y) = (f x, f y)
    uarrayToSet = fromList . uncurry zip . pair (UA.indices, UA.elems)
    (fs, os) = (tupply (Set.map fst) . partition snd . uarrayToSet . buildBitmap) t

drawMap prob = do
  putChar '+' >> putStr (Prelude.take (w+1) $ repeat '-') >> putStrLn "+"

  forM_ [h,h-1..0] $ \y -> do
    putChar '|'
    forM_ [0..w] $ \x -> do
      putChar $ drawCell ww (x, y)
    putStrLn "|"
    
  putChar '+' >> putStr (Prelude.take (w+1) $ repeat '-') >> putStrLn "+"
  where
    Right t = parseTask prob
    ww = initWW t
    (w, h) = maximum $ toList $ taskMap ww

drawCell ww pos =
  if taskPoint ww == pos then '@'
  else if member pos fs then ' '
  else if member pos os then '#'
  else '.'
  where
    me = ww
    fs = wwFrontier ww
    os = taskObstacles ww
