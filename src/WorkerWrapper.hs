module WorkerWrapper where

import Control.Arrow ((&&&))
import Data.Set
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
  WW { taskMap = convPoints t
     , taskPoint = p
     , taskObstacles = undefined -- TODO
     , taskBoosters = b
     , wwPosition = (0, 0)
     , wwArms = fromList [(0,0),(1,0),(1,1),(1,-1)]
     , wwBoosters = []
     , wwFrontier = undefined -- TODO
     , wwSpeed = 1
     }

convPoints :: Task -> Set Point
convPoints t = fromList $ Prelude.map fst $ Prelude.filter snd $ uncurry zip $ (UA.indices &&& UA.elems) (buildBitmap t)
