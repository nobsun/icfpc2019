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


boosterCodeP :: Parser BoosterCode
boosterCodeP = do
  c <- anyChar
  case c of
    'B' -> return BoosterB
    'F' -> return BoosterF
    'L' -> return BoosterL
    'X' -> return BoosterX
    _   -> fail ("Unknown booster code" ++ [c])
