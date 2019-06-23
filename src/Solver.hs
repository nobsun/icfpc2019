module Solver where

import Data.ByteString.Lazy as BL

import Task
import WorkerWrapper

readProb :: FilePath -> IO State
readProb path = return . initState =<< BL.readFile path
  
initState :: ByteString -> State
initState = either error initialState . parseTask . BL.init

-- 移動と転回のみ
simple = return . loop [] =<< readProb "examples/example-01.desc"
  where
    loop :: [Action] -> State -> [Action]
    loop ret s = maybe (Prelude.reverse ret) (\(a, s', e) -> loop (a:ret) s') (decide s 0)
