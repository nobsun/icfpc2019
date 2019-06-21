module Task
--    ( 
--    )
where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS





-----------------------------------------------------
-- 3.2 Encoding solutions

data Action
  = ActionW
  | ActionS
  | ActionA
  | ActionD
  | ActionZ
  | ActionE
  | ActionQ
  | ActionB (Int,Int)
  | ActionF
  | ActionL
  deriving (Eq, Ord)

instance Show Action where
  show ActionW = "W"
  show ActionS = "S"
  show ActionA = "A"
  show ActionD = "D"
  show ActionZ = "Z"
  show ActionE = "E"
  show ActionQ = "Q"
  show (ActionB (x,y)) = "B(" ++ show x ++ "," ++ show y ++ ")"
  show ActionF = "F"
  show ActionL = "L"


-----------------------------------------------------
-- 3.1 Task descriptions


type Point   = (Int, Int)
type TaskMap = [Point]

data Task =  Task
  { taskMap        :: TaskMap
  , taskPoint      :: Point
  , taskObstacles  :: [TaskMap]
  , taskBoosters   :: [(BoosterCode, Point)]
  } deriving (Show)


data BoosterCode
  = BoosterB
  | BoosterF
  | BoosterL
  | BoosterX
  deriving (Eq, Ord, Show)


-- | Parsing the task input. Here is an example from part-1/initial/prob-001.desc:
-- >>> parseTask (BS.pack "(15,23),(16,23),(16,17),(15,17),(15,20),(12,20),(12,19),(10,19),(10,16),(12,16),(12,17),(13,17),(13,14),(14,14),(14,8),(16,8),(16,15),(18,15),(18,0),(27,0),(27,15),(22,15),(22,23),(19,23),(19,25),(24,25),(24,27),(18,27),(18,33),(16,33),(16,32),(11,32),(11,30),(16,30),(16,27),(15,27),(15,25),(13,25),(13,24),(12,24),(12,29),(9,29),(9,27),(8,27),(8,37),(2,37),(2,27),(3,27),(3,24),(9,24),(9,23),(0,23),(0,22),(9,22),(9,21),(13,21),(13,22),(15,22)#(0,22)#(20,7),(24,7),(24,5),(22,5),(22,6),(21,6),(21,5),(19,5),(19,4),(20,4),(20,3),(19,3),(19,2),(20,2),(20,1),(21,1),(21,4),(22,4),(22,3),(23,3),(23,4),(24,4),(24,3),(25,3),(25,7),(26,7),(26,13),(24,13),(24,14),(23,14),(23,13),(22,13),(22,14),(21,14),(21,13),(20,13)#X(16,25);L(19,19);F(4,30);F(17,21);B(4,31)")
-- Right (Task {taskMap = [(15,23),(16,23),(16,17),(15,17),(15,20),(12,20),(12,19),(10,19),(10,16),(12,16),(12,17),(13,17),(13,14),(14,14),(14,8),(16,8),(16,15),(18,15),(18,0),(27,0),(27,15),(22,15),(22,23),(19,23),(19,25),(24,25),(24,27),(18,27),(18,33),(16,33),(16,32),(11,32),(11,30),(16,30),(16,27),(15,27),(15,25),(13,25),(13,24),(12,24),(12,29),(9,29),(9,27),(8,27),(8,37),(2,37),(2,27),(3,27),(3,24),(9,24),(9,23),(0,23),(0,22),(9,22),(9,21),(13,21),(13,22),(15,22)], taskPoint = (0,22), taskObstacles = [[(20,7),(24,7),(24,5),(22,5),(22,6),(21,6),(21,5),(19,5),(19,4),(20,4),(20,3),(19,3),(19,2),(20,2),(20,1),(21,1),(21,4),(22,4),(22,3),(23,3),(23,4),(24,4),(24,3),(25,3),(25,7),(26,7),(26,13),(24,13),(24,14),(23,14),(23,13),(22,13),(22,14),(21,14),(21,13),(20,13)]], taskBoosters = [(BoosterX,(16,25)),(BoosterL,(19,19)),(BoosterF,(4,30)),(BoosterF,(17,21)),(BoosterB,(4,31))]})

parseTask :: BS.ByteString -> Either String Task
parseTask s = do
  f (parse taskP s)
  where
    f (Done _i r)      = Right r
    f (Fail _i _cs e)  = Left e
    f (Partial k)      = f (k BS.empty)


taskP :: Parser Task
taskP = do
  m <- taskMapP
  _ <- char '#'
  p <- pointP
  _ <- char '#'
  o <- obstaclesP
  _ <- char '#'
  b <- boostersP
  return (Task m p o b)


pointP :: Parser Point
pointP = do
  _ <- char '('
  x <- decimal
  _ <- char ','
  y <- decimal
  _ <- char ')'
  return (x,y)


taskMapP :: Parser TaskMap
taskMapP = do
  sepBy pointP (char ',')


obstaclesP :: Parser [TaskMap]
obstaclesP = do
  sepBy taskMapP (char ';')


boostersP :: Parser [(BoosterCode, Point)]
boostersP = do
  sepBy boosterLocationP (char ';')


boosterLocationP :: Parser (BoosterCode, Point)
boosterLocationP = do
  c <- boosterCodeP
  p <- pointP
  return (c, p)



-- | Parsing BoosterCode.
-- >>> parse boosterCodeP (BS.pack "B")
-- Done "" BoosterB

boosterCodeP :: Parser BoosterCode
boosterCodeP = do
  c <- anyChar
  case c of
    'B' -> return BoosterB
    'F' -> return BoosterF
    'L' -> return BoosterL
    'X' -> return BoosterX
    _   -> fail ("Unknown booster code" ++ [c])
