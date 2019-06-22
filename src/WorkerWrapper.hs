module WorkerWrapper where

import Task

type Position = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Ord)

data WorkerWrapper = WW { -- 現在の位置
                          pos :: Position
                          -- 現在の向き
                        , dir :: Dir
                          -- 本体から到達可能な相対位置(濃い黄色のエリア)
                          -- ここから障害物や壁を考慮する
                        , touchable :: [Position]
                          -- 保有ブースター
                        , has :: [(BoosterCode, Int)]
                          -- 未踏の地
                        , frontier :: [Position]
                          -- 速度
                        , speed :: Int
                        } deriving (Show, Eq)
