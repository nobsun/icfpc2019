module Region
--  (
--  )
where


import Task (Point)



-- | (((x,y), pos, direction))
-- 点での表現から辺に変換するにあたり, (x,y)の両端が辺に含まれるように調整した.
-- 例: 点(0,0),点(0,5)を結ぶ辺は, 辺(0,4)になる.
--     ロボットが点(0,5)にいるときは, この辺(0,4)の上ではないので.
--
type Edge = (((Int, Int),Int), Direction)

data Direction = DirTop | DirLeft | DirBottom | DirRight
  deriving (Eq, Ord, Show)

data Region = Region
  { rgSize       :: Int
  , rgVertical   :: [Edge]
  , rgHorizontal :: [Edge]
  }
  deriving (Show)


-- | make Region from Point list.
-- >>> fromList [(0,0),(0,10),(10,10),(10,0)]
-- Region {rgSize = 9, rgVertical = [(((0,9),0),DirRight),(((0,9),10),DirLeft)], rgHorizontal = [(((0,9),10),DirBottom),(((0,9),0),DirTop)]}
-- >>> fromList [(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(8,3),(8,4),(6,4),(6,5),(0,5)]
-- Region {rgSize = 7, rgVertical = [(((0,0),6),DirRight),(((1,1),8),DirRight),(((2,2),6),DirRight),(((3,3),8),DirRight),(((4,4),6),DirRight),(((0,4),0),DirLeft)], rgHorizontal = [(((0,5),0),DirBottom),(((6,7),1),DirBottom),(((6,7),2),DirTop),(((6,7),3),DirBottom),(((6,7),4),DirTop),(((0,5),5),DirTop)]}
--

fromList :: [Point] -> Region
fromList []     =
  Region 0 [] []
fromList (x:xs) =
  let (s,vs,hs) = foldr f (0,[],[]) (zip (x:xs) (xs++[x]))
  in (Region (s-1) vs hs)
  where
    f :: ((Int,Int), (Int,Int)) -> (Int,[Edge],[Edge]) -> (Int,[Edge],[Edge])
    f ((x1,y1),(x2,y2)) (s,vs,hs) =
      (maximum [s,x1,y1,x2,y2]
      ,(if y1 > y2 then [(((y2,y1-1),x1), DirLeft)] else [])
         ++
       (if y2 > y1 then [(((y1,y2-1),x1), DirRight)] else [])
         ++
       vs
      ,(if x1 > x2 then [(((x2,x1-1),y1), DirTop)] else [])
        ++
       (if x2 > x1 then [(((x1,x2-1),y1), DirBottom)] else [])
        ++
       hs
      )


-- | Check whether point (x,y) is inside the region.
-- >>> isInside (fromList [(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(8,3),(8,4),(6,4),(6,5),(0,5)]) (6,1)
-- True
-- >>> isInside (fromList [(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(8,3),(8,4),(6,4),(6,5),(0,5)]) (6,2)
-- False
-- >>> isInside (fromList [(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(8,3),(8,4),(6,4),(6,5),(0,5)]) (6,3)
-- True

isInside :: Region -> Point -> Bool
isInside (Region _size vs hs) (x,y) =
  and [ not (null tops)
      , snd (minimum tops) == DirTop
      , not (null bottoms)
      , snd (maximum bottoms) == DirBottom
      , not (null rights)
      , snd (minimum rights) == DirRight
      , not (null lefts)
      , snd (maximum lefts) == DirLeft
      ]
  where
    -- 点(x,y)と範囲がかぶる辺だけ先に洗いだす
    relatedVs = [ (c,d) | (((a,b),c),d)<-vs, a<=y, y<=b]
    relatedHs = [ (c,d) | (((a,b),c),d)<-hs, a<=x, x<=b]
    -- 点(x,y)にもっとも近い四辺を決める
    tops    =  [(c,d)|(c,d)<-relatedHs, c> y]
    bottoms =  [(c,d)|(c,d)<-relatedHs, c<=y]
    rights  =  [(c,d)|(c,d)<-relatedVs, c> x]
    lefts   =  [(c,d)|(c,d)<-relatedVs, c<=x]


isOutside :: Region -> Point -> Bool
isOutside rg p =
  not (isInside rg p)

