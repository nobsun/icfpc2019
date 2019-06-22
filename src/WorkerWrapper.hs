module WorkerWrapper where

import Control.Arrow ((&&&))
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
     , wwPosition = (0, 0)
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
