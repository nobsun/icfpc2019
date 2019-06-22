-- yet another worker-wrapper implementation
module WorkerWrapper2 where

import Data.Array.Unboxed
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
  , stTeleportBeacons = Set.empty
  , stBoostersCollected = Map.empty
  , stBoostersOnMap = Map.fromListWith (++) [(p,[b]) | (b,p) <- taskBoosters task]
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


wrap :: UArray Point Bool -> WrapperState -> Set Point
wrap bm ws = Set.fromList $ p : [p1 | (xd,yd) <- Set.toList (wsManipulators ws), let p1 = (x+xd, y+yd), Bitmap.isVisible bm p p1]
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
          Map.fromList [(b,1) | b <- Map.findWithDefault [] (x1,y1) (stBoostersOnMap s), b /= BoosterX]
    , stBoostersOnMap =
        Map.adjust (\bs -> [b | b <- bs, b == BoosterX]) (x1,y1) (stBoostersOnMap s)
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
  | BoosterX `elem` Map.findWithDefault [] p (stBoostersOnMap s) = error "cannot install teleport beacon"
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
  | not (BoosterX `elem` Map.findWithDefault [] p (stBoostersOnMap s)) = error "not a spawn point"
  | otherwise =
      s
      { stUnwrapped = stUnwrapped s Set.\\ wrap (stMap s) w2
      , stBoostersCollected = Map.adjust (subtract 1) BoosterC (stBoostersCollected s)
      , stWrappers = stWrappers s `V.snoc` w2
      }
  where
    WrapperState{ wsPosition = p } = stWrappers s V.! i
    w2 = initialWrapperState p
