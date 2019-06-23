module WorkerWrapper where

import Data.Array.Unboxed
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Task
import qualified Bitmap


data WrapperState
  = WrapperState
  { wsPosition :: Point
  , wsFastWheelRemainingTime :: !Int
  , wsDrillRemainingTime :: !Int
  , wsManipulators :: Set Point -- 相対座標
  }
  deriving (Show)


data State
  = State
  { stMap :: UArray Point Bool -- True が通行可能で、 False がマップ外もしくは障害物
  , stUnwrapped :: Set Point
  , stSpawnPoints :: Set Point
  , stTeleportBeacons :: Set Point
  , stBoostersCollected :: Map BoosterCode Int
  , stBoostersOnMap :: Map Point [BoosterCode]
  , stWrappers :: Vector WrapperState
  }
  deriving (Show)


initialState :: Task -> State
initialState task =
  State
  { stMap = bm
  , stUnwrapped = us Set.\\ wrap bm w0
  , stSpawnPoints = Set.fromList [p | (b,p) <- taskBoosters task, b == BoosterX]
  , stTeleportBeacons = Set.empty
  , stBoostersCollected = Map.empty
  , stBoostersOnMap = Map.fromListWith (++) [(p,[b]) | (b,p) <- taskBoosters task, b /= BoosterX]
  , stWrappers = V.singleton w0
  }
  where
    w0 = initialWrapperState (taskPoint task)
    bm = Bitmap.buildBitmap task
    us = Set.fromList [(x,y) | ((x,y),b) <- assocs bm, b]


initialWrapperState :: Point -> WrapperState
initialWrapperState pos =
  WrapperState
  { wsPosition = pos
  , wsFastWheelRemainingTime = 0
  , wsDrillRemainingTime = 0
  , wsManipulators = initialManipulators
  }

initialManipulators :: Set Point
initialManipulators = Set.fromList [(1,0), (1,1), (1,-1)]


step :: [Action] -> State -> State
step as s
  | length as /= n = error "incorrect number of actions"
  | otherwise = foldl' (\s1 (i, a) -> stepWrapper i a s1) s (zip [0..] as)
  where
    n = V.length (stWrappers s)


simulateSolution :: [[Action]] -> State -> State
simulateSolution = loop
  where
    loop :: [[Action]] -> State -> State
    loop ass s =
      case splitAt n ass of
        (ass1, ass2) ->
          let (as, ass1') = unzip (map f ass1)
           in loop (ass1' ++ ass2) (step as s)
      where
        n = V.length (stWrappers s)

    f (a:as) = (a, as)
    f [] = (ActionZ, [])


wrap :: UArray Point Bool -> WrapperState -> Set Point
wrap bm ws = Set.fromList $ p : [p1 | (xd,yd) <- Set.toList (wsManipulators ws), let p1 = (x+xd, y+yd), inRange (bounds bm) p1, Bitmap.isVisible bm p p1]
  where
    p@(x,y) = wsPosition ws

stepWrapper :: Int -> Action -> State -> State
stepWrapper i a s = stepTime i $
  case a of
    ActionW -> move i (0,1) s
    ActionS -> move i (0,-1) s
    ActionA -> move i (-1,0) s
    ActionD -> move i (1,0) s
    ActionZ -> s
    ActionE -> turn i True s
    ActionQ -> turn i False s
    ActionB p -> attachNewManipulator i p s
    ActionF -> attachFastWheel i s
    ActionL -> useDrill i s
    ActionR -> reset i s
    ActionT p -> shift i p s
    ActionC -> clone i s


stepTime :: Int -> State -> State
stepTime i s =
  s
  { stWrappers = stWrappers s V.// [(i, w1)]
  }
  where
    WrapperState{ wsFastWheelRemainingTime = t1, wsDrillRemainingTime = t2 }  = stWrappers s V.! i
    w1 = w1{ wsFastWheelRemainingTime = min 0 t1, wsDrillRemainingTime = min 0 t2 }


move :: Int -> Point -> State -> State
move i d s
  | wsFastWheelRemainingTime w0 > 0 = move' i d $ move' i d s
  | otherwise = move' i d s
  where
    w0 = stWrappers s V.! i

-- TODO: 高速移動中に wrapping や アイテムの回収が可能なのかどうか確認する。
-- clones-v2.pdf の A Parallel Collection and Use of Boosters での記述的には、
-- 移動ステップで「移動先のアイテムを回収するのではなく」、次のステップで「現在の場所のアイテムを回収する」
-- という動きのような気がするが、そうすると高速移動中はアイテムを回収できないことになるのではないか?
move' :: Int -> Point -> State -> State
move' i (dx,dy) s
  | not (inRange (bounds (stMap s)) (x1,y1)) = s
  | not (stMap s ! (x1,y1)) && wsDrillRemainingTime w0 <= 0 = s
  | otherwise =
    s
    { stMap = map1
    , stUnwrapped = stUnwrapped s Set.\\ wrap map1 w1
    , stBoostersCollected =
        Map.unionWith (+) (stBoostersCollected s) $
          Map.fromList [(b,1) | b <- Map.findWithDefault [] (x1,y1) (stBoostersOnMap s)]
    , stBoostersOnMap =
        Map.delete (x1,y1) (stBoostersOnMap s)
    , stWrappers =
        stWrappers s V.// [(i, w1)]
    }
  where
    w0@WrapperState{ wsPosition = (x,y) } = stWrappers s V.! i
    x1 = x + dx
    y1 = y + dy
    map1 =
      if not (stMap s ! (x1,y1)) && wsDrillRemainingTime w0 > 0 then
        stMap s // [((x1,y1), True)]
      else
        stMap s
    w1 = w0{ wsPosition = (x1,y1) }


turn :: Int -> Bool -> State -> State
turn i isClockwise s =
  s
  { stUnwrapped = stUnwrapped s Set.\\ wrap (stMap s) w1
  , stWrappers = stWrappers s V.// [(i, w1)]
  }
  where
    w0 = stWrappers s V.! i
    w1 = w0{ wsManipulators = Set.map f (wsManipulators w0) }
    f (x,y)
      | isClockwise = (y, -x)
      | otherwise   = (-y, x)


attachNewManipulator :: Int -> (Int,Int) -> State -> State
attachNewManipulator i (dx,dy) s
  | Set.null (ms `Set.intersection` Set.fromList [(x1+1, y1), (x1-1, y1), (x1, y1+1), (x1, y1-1)]) = error "cannot attach"
  | otherwise =
      s
      { stUnwrapped = stUnwrapped s Set.\\ wrap (stMap s) w1
      , stWrappers  = stWrappers s V.// [(i, w1)]
      }
  where
    w0@WrapperState{ wsPosition = (x,y), wsManipulators = ms } = stWrappers s V.! i
    x1 = x + dx
    y1 = y + dy
    w1 = w0{ wsManipulators = Set.insert (x1,y1) ms }


attachFastWheel :: Int -> State -> State
attachFastWheel i s
  | Map.findWithDefault 0 BoosterF (stBoostersCollected s) <= 0 = error "fast wheel not available"
  | otherwise =
      s
      { stBoostersCollected = Map.adjust (subtract 1) BoosterF (stBoostersCollected s)
      , stWrappers  = stWrappers s V.// [(i, w1)]
      }
  where
    w0@WrapperState{ wsFastWheelRemainingTime = t } = stWrappers s V.! i
    w1 = w0{ wsFastWheelRemainingTime = max 51 t } -- ステップの最後で減らすので1多い値にしておく


useDrill :: Int -> State -> State
useDrill i s
  | Map.findWithDefault 0 BoosterL (stBoostersCollected s) <= 0 = error "drill not available"
  | otherwise =
      s
      { stBoostersCollected = Map.adjust (subtract 1) BoosterL (stBoostersCollected s)
      , stWrappers = stWrappers s V.// [(i, w1)]
      }
  where
    w0@WrapperState{ wsDrillRemainingTime = t } = stWrappers s V.! i
    w1 = w0{ wsDrillRemainingTime = max 51 t } -- ステップの最後で減らすので1多い値にしておく


reset :: Int -> State -> State
reset i s
  | Map.findWithDefault 0 BoosterR (stBoostersCollected s) <= 0  = error "reset not available"
  | p `Set.member` stTeleportBeacons s = error "cannot install teleport beacon"
  | p `Set.member` stSpawnPoints s = error "cannot install teleport beacon"
  | otherwise =
      s
      { stBoostersCollected = Map.adjust (subtract 1) BoosterR (stBoostersCollected s)
      , stTeleportBeacons = Set.insert p (stTeleportBeacons s)
      }
  where
    WrapperState{ wsPosition = p } = stWrappers s V.! i


shift :: Int -> Point -> State -> State
shift i p s
  | p `Set.notMember` stTeleportBeacons s = error "invalid destination"
  | otherwise =
      s
      { stUnwrapped = stUnwrapped s Set.\\ wrap (stMap s) w1
      , stWrappers = stWrappers s V.// [(i, w1)]
      }
  where
    w0 = stWrappers s V.! i
    w1 = w0{ wsPosition = p }


clone :: Int -> State -> State
clone i s
  | Map.findWithDefault 0 BoosterC (stBoostersCollected s) <= 0 = error "cloning not available"
  | p `Set.notMember` stSpawnPoints s = error "not a spawn point"
  | otherwise =
      s
      { stUnwrapped = stUnwrapped s Set.\\ wrap (stMap s) w2
      , stBoostersCollected = Map.adjust (subtract 1) BoosterC (stBoostersCollected s)
      , stWrappers = stWrappers s `V.snoc` w2
      }
  where
    WrapperState{ wsPosition = p } = stWrappers s V.! i
    w2 = initialWrapperState p

-------------------------------------------------------------------------------------------
-- high level API
-------------------------------------------------------------------------------------------
possibleActions :: State -> Vector (WrapperState, [Action])
possibleActions s = V.map (possible s) (stWrappers s)
  where
    possible :: State -> WrapperState -> (WrapperState, [Action])
    possible s ws = (ws, moves ++ turns ++ drill ++ speedup ++ extendarms ++ clone ++ reset ++ shift)
      where
        pos@(x,y) = wsPosition ws
        -- 移動候補
        moves = [a | (p, a) <- [((x,y+1), ActionW),((x+1,y), ActionD),((x,y-1), ActionS),((x-1,y), ActionA)]
                   , inRange (bounds (stMap s)) p, stMap s ! p]
        -- 転回候補
        turns = [ActionE, ActionQ]
        -- スピードアップ
        speedup = maybe [] (\n -> if n>0 then [ActionF] else []) (Map.lookup BoosterF (stBoostersCollected s))
        -- ドリル使用
        drill = maybe [] (\n -> if n>0 then [ActionL] else []) (Map.lookup BoosterL (stBoostersCollected s))
        -- クローン
        clone = maybe [] (\n -> if n>0 then [ActionC] else []) (Map.lookup BoosterC (stBoostersCollected s))
        -- リセット
        reset = maybe [] (\n -> if n>0 then [ActionR] else []) (Map.lookup BoosterR (stBoostersCollected s))
        -- シフト
        shift = map ActionT (Set.toList (stTeleportBeacons s))
        -- マニピュレータ追加
        extendarms = map ActionB (Set.toList $ arounds Set.\\ wsbody)
        wsbody = Set.insert pos (wsManipulators ws)
        arounds = Set.unions (Set.toList (Set.map around wsbody))
          where
            around :: Point -> Set Point
            around (x', y') = Set.fromList [(x',y'+1),(x'+1,y'),(x',y'-1),(x'-1,y')]
