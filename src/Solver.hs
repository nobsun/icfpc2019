module Solver where

import Data.ByteString.Lazy as BL

import Task
import WorkerWrapper

readProb :: FilePath -> IO State
readProb path = return . initState =<< BL.readFile path
  
initState :: ByteString -> State
initState = either error initialState . parseTask . BL.init
