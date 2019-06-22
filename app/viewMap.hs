
import Data.Array.Unboxed (bounds)
import System.Environment (getArgs)


import Bitmap
import Problems

dump :: (Show a, Integral a) => a -> IO ()
dump n = do
  task <- readTaskFile n
  let bm = buildBitmap task
  print $ bounds bm
  printBitmap bm

main :: IO ()
main = do
  as <- getArgs
  n <- case as of
    []   ->  fail "problem number required."
    a:_  ->  readIO a :: IO Int
  dump n
