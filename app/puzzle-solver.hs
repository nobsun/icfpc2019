{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Options.Applicative
import System.Exit
import System.IO

import qualified PuzzleSolver
import Task

data Options
  = Options
  { optInput :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> fileInput
  where
    fileInput :: Parser FilePath
    fileInput = strArgument $ metavar "FILE"

parserInfo :: ParserInfo Options
parserInfo = info optionsParser
  $  fullDesc
  <> header "puzzle-solver - a solver for .cond files"

main :: IO ()
main = do
  opt <- execParser parserInfo
  s <- LB.readFile $ optInput opt
  case parsePuzzle s of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right puzzle -> do
      m <- PuzzleSolver.solve puzzle
      case m of
        Nothing -> hPutStrLn stderr "failed to solve" >> exitFailure
        Just task -> do
          BB.hPutBuilder stdout (printTask task)
          SB.putStrLn ""
