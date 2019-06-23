{-# LANGUAGE OverloadedStrings #-}

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
import Data.Attoparsec.ByteString.Lazy
  (Parser, parse, eitherResult)
import Data.Attoparsec.ByteString.Char8 hiding (parse, eitherResult)
import qualified Data.ByteString.Lazy.Char8 as L8





-----------------------------------------------------
-- 3.2 Encoding solutions

type PrintM = Writer (DList L8.ByteString)
type Printer a = a -> PrintM ()

runPrinter :: Printer a -> a -> L8.ByteString
runPrinter p = mconcat . DList.toList . execWriter . p

data Action
  = MoveUp             -- ^ move up
  | MoveDown           -- ^ move down
  | MoveLeft           -- ^ move left
  | MoveRight          -- ^ move right
  | NoOp               -- ^ do nothing
  | TurnCW             -- ^ turn 90 deg CW
  | TurnCCW            -- ^ turn 90 deg CCW
  | AttachManipulator (Int,Int)  -- ^ attach manipulator
  | UseFastWheel       -- ^ attach fast wheel
  | UseDrill           -- ^ start using drill
  | Reset              -- ^ reset beacon
  | Shift (Int, Int)   -- ^ shift location
  | Clone              -- ^ clone
  deriving (Eq, Ord, Show)

encodeAction :: Action -> L8.ByteString
encodeAction = enc
  where
    enc MoveUp = "W"
    enc MoveDown = "S"
    enc MoveLeft = "A"
    enc MoveRight = "D"
    enc NoOp = "Z"
    enc TurnCW = "E"
    enc TurnCCW = "Q"
    enc (AttachManipulator (x,y)) = "B(" <> L8.pack (show x) <> "," <> L8.pack (show y) <> ")"
    enc UseFastWheel = "F"
    enc UseDrill = "L"
    enc Reset = "R"
    enc (Shift (x,y)) = "T(" <> L8.pack (show x) <> "," <> L8.pack (show y) <> ")"
    enc Clone = "C"

printAction :: Printer Action
printAction a = tell . pure $ encodeAction a

type Actions = [Action]

printActions :: Printer Actions
printActions = mapM_ printAction

printActionDList :: Printer (DList Action)
printActionDList = mapM_ printAction . DList.toList

---

actionsP :: Parser Actions
actionsP = many actionP

actionP :: Parser Action
actionP = msum
  [ char 'W' *> pure MoveUp
  , char 'S' *> pure MoveDown
  , char 'A' *> pure MoveLeft
  , char 'D' *> pure MoveRight
  , char 'Z' *> pure NoOp
  , char 'E' *> pure TurnCW
  , char 'Q' *> pure TurnCCW
  , AttachManipulator <$> (char 'B' *> pointP)
  , char 'F' *> pure UseFastWheel
  , char 'L' *> pure UseDrill
  , char 'R' *> pure Reset
  , Shift <$> (char 'T' *> pointP)
  , char 'C' *> pure Clone
  ]

type Solution = [Actions]

printSolution :: Printer Solution
printSolution = sequence_ . intersperse (tell (pure "#")) . map printActions

parseSolution :: L8.ByteString -> Either String Solution
parseSolution = runParser solutionP

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
  = BoosterB -- ^ manipulator
  | BoosterF -- ^ fast wheel
  | BoosterL -- ^ drill
  | BoosterX -- ^ spawn point
  | BoosterR -- ^ teleports
  | BoosterC -- ^ clone
  deriving (Eq, Ord, Show)


-- | Parsing the task input. Here is an example from part-1/initial/prob-001.desc:
-- >>> parseTask (L8.pack "(15,23),(16,23),(16,17),(15,17),(15,20),(12,20),(12,19),(10,19),(10,16),(12,16),(12,17),(13,17),(13,14),(14,14),(14,8),(16,8),(16,15),(18,15),(18,0),(27,0),(27,15),(22,15),(22,23),(19,23),(19,25),(24,25),(24,27),(18,27),(18,33),(16,33),(16,32),(11,32),(11,30),(16,30),(16,27),(15,27),(15,25),(13,25),(13,24),(12,24),(12,29),(9,29),(9,27),(8,27),(8,37),(2,37),(2,27),(3,27),(3,24),(9,24),(9,23),(0,23),(0,22),(9,22),(9,21),(13,21),(13,22),(15,22)#(0,22)#(20,7),(24,7),(24,5),(22,5),(22,6),(21,6),(21,5),(19,5),(19,4),(20,4),(20,3),(19,3),(19,2),(20,2),(20,1),(21,1),(21,4),(22,4),(22,3),(23,3),(23,4),(24,4),(24,3),(25,3),(25,7),(26,7),(26,13),(24,13),(24,14),(23,14),(23,13),(22,13),(22,14),(21,14),(21,13),(20,13)#X(16,25);L(19,19);F(4,30);F(17,21);B(4,31)")
-- Right (Task {taskMap = [(15,23),(16,23),(16,17),(15,17),(15,20),(12,20),(12,19),(10,19),(10,16),(12,16),(12,17),(13,17),(13,14),(14,14),(14,8),(16,8),(16,15),(18,15),(18,0),(27,0),(27,15),(22,15),(22,23),(19,23),(19,25),(24,25),(24,27),(18,27),(18,33),(16,33),(16,32),(11,32),(11,30),(16,30),(16,27),(15,27),(15,25),(13,25),(13,24),(12,24),(12,29),(9,29),(9,27),(8,27),(8,37),(2,37),(2,27),(3,27),(3,24),(9,24),(9,23),(0,23),(0,22),(9,22),(9,21),(13,21),(13,22),(15,22)], taskPoint = (0,22), taskObstacles = [[(20,7),(24,7),(24,5),(22,5),(22,6),(21,6),(21,5),(19,5),(19,4),(20,4),(20,3),(19,3),(19,2),(20,2),(20,1),(21,1),(21,4),(22,4),(22,3),(23,3),(23,4),(24,4),(24,3),(25,3),(25,7),(26,7),(26,13),(24,13),(24,14),(23,14),(23,13),(22,13),(22,14),(21,14),(21,13),(20,13)]], taskBoosters = [(BoosterX,(16,25)),(BoosterL,(19,19)),(BoosterF,(4,30)),(BoosterF,(17,21)),(BoosterB,(4,31))]})

parseTask :: L8.ByteString -> Either String Task
parseTask = runParser taskP


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
-- >>> parse boosterCodeP (L8.pack "B")
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

-----

runParser :: Parser a -> L8.ByteString -> Either String a
runParser p = eitherResult . parse (many space *> p <* many space <* endOfInput)
