module Task
--    (
--    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.List
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS





-----------------------------------------------------
-- 3.2 Encoding solutions

type PrintM = Writer (DList Char)
type Printer a = a -> PrintM ()

runPrinter :: Printer a -> a -> String
runPrinter p = DList.toList . execWriter . p

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
  | ActionR
  | ActionT (Int, Int)
  | ActionC
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
  show ActionR = "R"
  show (ActionT (x,y)) = "T(" ++ show x ++ "," ++ show y ++ ")"
  show ActionC = "C"

printAction :: Printer Action
printAction a = tell . DList.fromList $ show a

type Actions = [Action]

printActions :: Printer Actions
printActions = mapM_ printAction

printActionDList :: Printer (DList Action)
printActionDList = mapM_ printAction . DList.toList

parseActions :: BS.ByteString -> Either String Actions
parseActions = parseOnly actionsP

actionsP :: Parser Actions
actionsP = many actionP

actionP :: Parser Action
actionP = msum
  [ char 'W' *> pure ActionW
  , char 'S' *> pure ActionS
  , char 'A' *> pure ActionA
  , char 'D' *> pure ActionD
  , char 'Z' *> pure ActionZ
  , char 'E' *> pure ActionE
  , char 'Q' *> pure ActionQ
  , ActionB <$> (char 'B' *> pointP)
  , char 'F' *> pure ActionF
  , char 'L' *> pure ActionL
  , char 'R' *> pure ActionR
  , ActionT <$> (char 'T' *> pointP)
  , char 'C' *> pure ActionC
  ]

type Solution = [Actions]

printSolution :: Printer Solution
printSolution = sequence_ . intersperse (tell (DList.singleton '#')) . map printActions

parseSolution :: BS.ByteString -> Either String Solution
parseSolution = parseOnly solutionP

solutionP :: Parser Solution
solutionP = sepBy actionsP (char '#')

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
  | BoosterR
  | BoosterC
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
taskP =
  Task
  <$> taskMapP    <* char '#'
  <*> pointP      <* char '#'
  <*> obstaclesP  <* char '#'
  <*> boostersP


pointP :: Parser Point
pointP = (,) <$> (char '(' *> decimal) <* char ',' <*> decimal <* char ')'


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
boosterLocationP =
  (,) <$> boosterCodeP <*> pointP


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
    'R' -> return BoosterR
    'C' -> return BoosterC
    _   -> fail ("Unknown booster code" ++ [c])
