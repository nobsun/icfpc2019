{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Options.Applicative
import System.Exit
import System.IO

import qualified SolverSimple
import qualified SolverSimplePrime
import qualified SolverGetManipulator
import Task

data Options
  = Options
  { optInput :: FilePath
  , optAlgorithm :: String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> fileInput
  <*> algorithmOption
  where
    fileInput :: Parser FilePath
    fileInput = strArgument $ metavar "FILE"

    algorithmOption :: Parser String
    algorithmOption = strOption
      $  long "alg"
      <> metavar "ALGORITHM"
      <> help "algorithm: simple (default), simple-prime, get-manip"
      <> value "simple"

parserInfo :: ParserInfo Options
parserInfo = info optionsParser
  $  fullDesc
  <> header "solver - a solver for .desc files"


main :: IO ()
main = do
  opt <- execParser parserInfo
  s <- LB.readFile $ optInput opt
  case parseTask s of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right task -> do
      sol <- case optAlgorithm opt of
               "simple" -> return $ SolverSimple.solve task
               "simple-prime" -> return $ SolverSimplePrime.solve task
               "get-manip" -> return $ SolverGetManipulator.solve task
               name -> hPutStrLn stderr ("unknown solver: " ++ name) >> exitFailure
      BB.hPutBuilder stdout (printSolution sol)
      SB.putStrLn ""
