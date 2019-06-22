module WorkerWrapper where

import Task
import Region

type Position = (Int, Int)

data WorkerWrapper = WW { -- 現在の位置
                          pos :: Position
                          -- 現在の向き
                        , dir :: Direction
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
