module Problems where

import qualified Data.ByteString.Lazy as LB
import System.FilePath ((</>), (<.>))

import Task (Task, parseTask)

-----

probInitialDir :: FilePath
probInitialDir = "/home/icfpc2019/problems/part-1-initial"

probTeleportsDir :: FilePath
probTeleportsDir = "/home/icfpc2019/problems/part-2-teleports"

probClonesDir :: FilePath
probClonesDir = "/home/icfpc2019/problems/part-3-clones"

-----

readTaskFile :: (Show a, Integral a)
             => a -> IO Task
readTaskFile n = do
  path <- maybe (fail $ "unknown problem number: " ++ show n) return $ problemPath n
  cont <- LB.readFile path
  either (fail . ("parse error in " <> path <> ": " ++)) return $ parseTask cont

-----

data ProblemType
  = ProbInitial
  | ProbTeleports
  | ProbClones
  deriving (Eq, Ord, Show, Read)

problemType :: (Ord a, Integral a)
            => a
            -> Maybe ProblemType
problemType n
  | n <= 0    =  Nothing
  | n <= 150  =  Just ProbInitial
  | n <= 220  =  Just ProbTeleports
  | n <= 300  =  Just ProbClones
  | otherwise =  Nothing

problemDir :: ProblemType -> FilePath
problemDir = d where
  d ProbInitial   = probInitialDir
  d ProbTeleports = probTeleportsDir
  d ProbClones    = probClonesDir

ddd :: Show a => a -> String
ddd n = replicate (3 - length s) '0' <> s
  where
    s = show n

problemPath :: (Ord a, Integral a, Show a)
            => a
            -> Maybe FilePath
problemPath n = do
  t <- problemType n
  return $ problemDir t </> ("prob-" <> ddd n) <.> "desc"
