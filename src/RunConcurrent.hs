module RunConcurrent  (
  solveConcurrent,
  ) where

import Control.Monad
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

import RunSolver (Algo, callSolver)

data TaskQueue a =
  TaskQueue
  { tasks :: Chan (Maybe a)
  , waits :: Chan ()
  }

newTaskQueue :: IO (TaskQueue a)
newTaskQueue = TaskQueue <$> newChan <*> newChan

enqueueTask :: TaskQueue a -> a -> IO ()
enqueueTask q = writeChan (tasks q) . Just

finalizeTaskQueue :: Int -> TaskQueue a -> IO ()
finalizeTaskQueue n q = do
  replicateM_ n (writeChan (tasks q) Nothing)
  replicateM_ n (readChan $ waits q)

runConcurrent :: Int
              -> TaskQueue a
              -> (a -> IO b)
              -> IO [ThreadId]
runConcurrent n q action =
    replicateM n $ forkIO loop
  where
    loop = do
      mt <- readChan $ tasks q
      maybe (writeChan (waits q) ()) (\t -> action t >> loop) mt

-----

solveConcurrent :: Int
                -> [(Int, Algo)]
                -> IO ()
solveConcurrent para probs = do
  tq <- newTaskQueue
  void $ runConcurrent para tq (uncurry callSolver)
  mapM_ (enqueueTask tq) probs
  finalizeTaskQueue para tq
