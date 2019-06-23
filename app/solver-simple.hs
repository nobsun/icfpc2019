module Main where

import qualified Data.ByteString.Lazy.Char8 as LB
import System.Environment
import System.Exit
import System.IO

import SolverSimple
import Task


main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> fail "problem filename required."
    fname : _  -> do
      s <- LB.readFile fname
      case parseTask s of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right task -> do
          let sol = solve task
          LB.putStrLn $ runPrinter printSolution sol
