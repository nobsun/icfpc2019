module RunSolver where

import qualified Data.ByteString.Builder as BB
import System.FilePath ((</>), (<.>))
import System.Directory (renameFile, createDirectoryIfMissing)
import System.Process (rawSystem)
import System.IO (withFile, IOMode (WriteMode))

import Task (Task, Solution, printSolution)
import qualified SolverSimple
import qualified SolverSimplePrime
import qualified SolverGetManipulator
import Problems (problemName, readTaskFile)
import ProcessIO (ioExitCode)

data Algo
  = Simple
  | SimplePrime
  | GetManipulator
  deriving (Eq, Ord, Show, Read)

solve1 :: (Integral a, Show a)
       => Algo
       -> a
       -> IO ()
solve1 algo n = do
  task <- readTaskFile n
  let sol = doSolve algo task
      solFn = solutionDir algo </> problemName n <.> "sol"
  createDirectoryIfMissing True (solutionDir algo)
  withFile (solFn <.> "new") WriteMode
    $ \h -> BB.hPutBuilder h (printSolution sol)
  renameFile (solFn <.> "new") solFn

doSolve :: Algo -> Task -> Solution
doSolve Simple         =  SolverSimple.solve
doSolve SimplePrime    =  SolverSimplePrime.solve
doSolve GetManipulator =  SolverGetManipulator.solve

solutionDir :: Algo -> FilePath
solutionDir = ("/home/icfpc2019/solutions/" </>) . show

callSolver :: Int -> Algo -> IO ()
callSolver n a =
  ioExitCode =<< rawSystem "./lib/do-solve-one.sh" [show n, show a]
