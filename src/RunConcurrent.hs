module RunConcurrent  (
  newTaskQueue, enqueueTask, runConcurrent,
  ) where

import Control.Monad
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

type TaskQueue a = Chan (Maybe a)

newTaskQueue :: IO (TaskQueue a)
newTaskQueue = newChan

enqueueTask :: TaskQueue a -> a -> IO ()
enqueueTask q = writeChan q . Just

runConcurrent :: Int
              -> TaskQueue a
              -> (a -> IO b)
              -> IO [ThreadId]
runConcurrent n q action =
    replicateM n $ forkIO loop
  where
    loop = do
      mt <- readChan q
      maybe (return ()) (\t -> action t >> loop) mt
