module WorkerWrapper where

import Task hiding (taskMap, taskPoint, taskObstacles, taskBoosters)

type Velocity = Int

data WorkerWrapper = WW
  { taskMap       :: TaskMap
  , taskPoint     :: Point
  , taskObstacles :: [TaskMap]
  , taskBoosters  :: [(BoosterCode, Point)]

  , wwPosition    :: Point
  , wwArms        :: TaskMap
  , wwBoosters    :: [(BoosterCode, Int)]
  , wwFrontier    :: TaskMap
  , wwSpeed       :: Velocity
  } deriving (Show, Eq, Ord)

initWW :: Task -> WorkerWrapper
initWW (Task m p o b) =
  WW { taskMap = m
     , taskPoint = p
     , taskObstacles = o
     , taskBoosters = b
     , wwPosition = (0, 0)
     , wwArms = [(0,0),(1,0),(1,1),(1,-1)]
     , wwFrontier = []
     , wwSpeed = 1
     }
