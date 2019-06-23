module Main where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import System.Directory
import System.Environment
import System.Exit
import System.IO

solDirs :: (FilePath, FilePath)
-- solDirs = ("solutions-merged/","solutions-simple-solver/")
solDirs = ("solutions-merged/","solutions-solver-simpleprime/")
(oldDir, newDir) = solDirs

mkNNN :: Int -> String
mkNNN n | 1 <= n && n <10 = "00" ++ show n
        | 10 <= n && n <= 99 = "0" ++ show n
        | 100 <= n = show n

tupply f (x, y) = (f x, f y)

timeSize :: FilePath -> IO Int
timeSize file = do
  exist <- doesFileExist file
  if exist
    then readFile file >>= return . timeSize'
    else return maxBound

timeSize' :: String -> Int
timeSize' = sub 0
  where
    sub :: Int -> String -> Int
    sub n [] = n
    sub n (c:cs) | c `elem` ("WSADZEQBFLRC" :: String) = sub (n+1) cs
                 | otherwise = sub n cs

main :: IO ()
main = do
  forM_ (map mkNNN [1..150]) $ \nnn -> do
    let (old, new) = (f nnn oldDir, f nnn newDir)
    (szo, szn) <- (,) <$> timeSize old <*> timeSize new
    if szo > szn
      then copyFile new old
      else return ()
  where
    f n d = d ++ "prob-" ++ n ++ ".sol"
