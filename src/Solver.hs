module Solver where

import Data.ByteString.Lazy

import Task
import WorkerWrapper

initState :: ByteString -> State
initState prob = s
  where
    Right t = parseTask prob
    s = initialState t
