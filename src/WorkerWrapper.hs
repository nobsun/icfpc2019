module WorkerWrapper where

import Task hiding (TaskMap, taskMap, taskPoint, taskObstacles, taskBoosters)
import Data.Set

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
initWW (Task m p o b) =
  WW { taskMap = undefined
     , taskPoint = p
     , taskObstacles = undefined -- TODO
     , taskBoosters = b
     , wwPosition = (0, 0)
     , wwArms = fromList [(0,0),(1,0),(1,1),(1,-1)]
     , wwBoosters = []
     , wwFrontier = undefined -- TODO
     , wwSpeed = 1
     }
