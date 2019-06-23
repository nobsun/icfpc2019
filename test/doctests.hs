module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Task.hs"
  , "src/Region.hs"
  ]
