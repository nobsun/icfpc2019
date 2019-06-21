module Lib
    ( someFunc
    ) where

-- | 「なんか関数」を標準出力に印字する
-- >>> someFunc
-- なんか関数

someFunc :: IO ()
someFunc = putStrLn "なんか関数" --- "なんか函数"
