module RunConcurrent  (
  solveConcurrent,
  ) where

import Control.Monad
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)

import RunSolver (Algo, callSolver)

type TaskQueue a = Chan (Maybe a)

newTaskQueue :: IO (TaskQueue a)
newTaskQueue = newChan

enqueueTask :: TaskQueue a -> a -> IO ()
enqueueTask q = writeChan q . Just

finalizeTaskQueue :: Int -> TaskQueue a -> IO ()
finalizeTaskQueue n q = replicateM_ n (writeChan q Nothing)

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

-----

solveConcurrent :: Int
                -> [(Int, Algo)]
                -> IO ()
solveConcurrent para probs = do
  tq <- newTaskQueue
  void $ runConcurrent para tq (uncurry callSolver)
  mapM_ (enqueueTask tq) probs
  finalizeTaskQueue para tq
