{-# OPTIONS -Wall #-}

module SubmitQueue (
  submitGateway,
  ) where

import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import System.IO.Error (tryIOError)
import Data.List (isSuffixOf)
import Data.Time
  (NominalDiffTime, diffUTCTime, addUTCTime, getCurrentTime,
   getZonedTime, utcToLocalZonedTime, FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import System.IO (stdout, BufferMode (LineBuffering), hSetBuffering, hGetContents)
import System.FilePath ((</>), (<.>))
import System.Directory (renameFile)
import System.Process (rawSystem, runInteractiveProcess)
import System.Exit (ExitCode (..))


submitGateway :: IO ()
submitGateway = do
  hSetBuffering stdout LineBuffering
  putLog <- do
    put <- newLog
    return $ \s -> do
      ts <- formatLogStamp <$> getZonedTime
      put $ ts ++ ": " ++ s
  queue  <- newChan
  void . forkIO . submitLoop putLog $ submit queue
  waitRequest putLog queue

interval :: NominalDiffTime
interval = fromInteger $ 10 * 60

newLog :: IO (String -> IO ())
newLog = do
  c <- newChan
  void . forkIO . forever $ putStrLn =<< readChan c
  return $ writeChan c

formatLogStamp :: FormatTime t => t -> String
formatLogStamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

submitLoop :: (String -> IO ()) -> IO () -> IO ()
submitLoop putLog_ submit_ =
    loop Nothing
  where
    putLog = putLog_ . ("submit: " ++)
    loop maySubmitTs = do
      let delayRest prev = do
            current <- getCurrentTime
            let waitt = interval - diffUTCTime current prev
            prevL <- utcToLocalZonedTime prev
            nextL <- utcToLocalZonedTime $ addUTCTime waitt current
            putLog $ unwords
              [ "previouns post at",
                formatLogStamp prevL ++ ".",
                "so, wait", show waitt ++ ".",
                "will proceed at",
                formatLogStamp nextL ++ "." ]
            threadDelay $ fromEnum waitt `quot` 1000000
      maybe (return ()) delayRest maySubmitTs
      putLog "now, dequeue next submit."
      putLog . either (\e -> "submit failed: " ++ show e) (\() -> "submit done.")
        =<< tryIOError submit_
      loop . Just =<< getCurrentTime

formatFileStamp :: FormatTime t => t -> String
formatFileStamp = formatTime defaultTimeLocale "%d-%H%M%S"

-- post --> queue --> submit

postDir :: FilePath
postDir = "/home/icfpc2019/submit/post"

queueDir :: FilePath
queueDir = "/home/icfpc2019/submit/queue"

submitDir :: FilePath
submitDir = "/home/icfpc2019/submit/submit"

submit :: Chan FilePath -> IO ()
submit q = do
  fn <- readChan q
  ts <- formatFileStamp <$> getZonedTime
  let dfn = ts ++ "_" ++ fn
  renameFile (queueDir </> fn) (submitDir </> dfn)
  ioExitCode =<< rawSystem "./lib/do-submit.sh" [submitDir </> dfn]


waitRequest :: (String -> IO ()) -> Chan FilePath -> IO ()
waitRequest putLog_ q = do
  let putLog = putLog_ . ("request: " ++)
      process req = case req of
        [_d, _ev, fn] | ".zip" `isSuffixOf` fn -> do
          ts <- formatFileStamp <$> getZonedTime
          let qfn = ts <.> "zip"
          putLog $ "enqueue: " ++ fn ++ " --> " ++ qfn
          renameFile (postDir </> fn) (queueDir </> qfn)
          writeChan q qfn
        [_d, _ev, fn] ->
          putLog $ "not zip, ignored: " ++ fn
        _             -> return ()
  reqs <- map words . lines
          <$> readInotifyEvents
  mapM_ process reqs

readInotifyEvents :: IO String
readInotifyEvents = do
  (_in', out, _err, _ph) <- runInteractiveProcess
                            "/usr/bin/inotifywait" ["-m", "-e", "close_write", postDir]
                            Nothing Nothing
  hSetBuffering out LineBuffering
  hGetContents out

runExitCode :: a -> (Int -> a) -> ExitCode -> a
runExitCode s e ec = case ec of
  ExitSuccess   -> s
  ExitFailure c -> e c

ioExitCode :: ExitCode -> IO ()
ioExitCode = runExitCode (return ()) (fail . ("exited failure with code: " ++) . show)
