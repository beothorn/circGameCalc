module Main (main) where

--------------------------------------------------------
-- All non intersecting, non rotated lines for points --
--                                                    --
--                     -1,2    1,2                    --
--              -2,2 .___x______x___                  --
--                   |              |                 --
--              -2,1 x              x 2,1             --
--                   |     0,0      |                 --
--                   |              |                 --
--             -2,-1 x              x 2,-1            --
--                   |___x______x___|.2,-1            --
--                     -1,-2    1,-2                  --
--------------------------------------------------------

data Point' = Point {x :: Float, y :: Float} deriving (Show)

instance Eq Point' where
  (Point x y) == (Point x2 y2) = (x-x2 < 0.01) && (y-y2 < 0.01)

topLeft =     Point (-1.0)  2.0
topRight =    Point   1.0   2.0
rightTop =    Point   2.0   1.0
rightBottom = Point   2.0  (-1.0)
bottomRight = Point   1.0  (-2.0)
bottomLeft =  Point (-1.0) (-2.0)
leftBottom =  Point (-2.0) (-1.0)
leftTop =     Point (-2.0)   1.0

thePoints :: [Point']
thePoints = [topRight
  ,topLeft
  ,rightTop
  ,rightBottom
  ,bottomRight
  ,bottomLeft
  ,leftBottom
  ,leftTop]

data LineSegment' = LineSegment Point' Point' deriving (Show)

instance Eq LineSegment' where
  (LineSegment l1p1 l1p2) == (LineSegment l2p1 l2p2) = (l1p1 == l2p1 && l1p2 == l2p2) || (l1p2 == l2p1 && l1p1 == l2p2)

allPossibleLinesFor :: [Point'] -> [LineSegment']
allPossibleLinesFor [] = []
allPossibleLinesFor (firstPoint:pointsLeft) = allLinesForFirstPoint ++ allPossibleLinesFor pointsLeft where
 allLinesForFirstPoint = map (LineSegment firstPoint) pointsLeft

isPointInsideRectangle :: Point' -> Bool
isPointInsideRectangle (Point x y) = (x >=0) && (x<=3) && (y>=0) && (y<=3)

-- line equation y = ax + b
aFor :: LineSegment' -> Float
aFor (LineSegment (Point x y) (Point x2 y2)) = (y2 - y) / (x2 - x)

bFor :: LineSegment' -> Float
bFor line@(LineSegment (Point x y) (Point x2 y2)) = y - (aFor line) * x

maybeCommonPointFor :: LineSegment' -> LineSegment' -> Maybe Point'
maybeCommonPointFor (LineSegment l1p1 l1p2) (LineSegment l2p1 l2p2)
  | l1p1 == l2p1 = Just l1p1
  | l1p1 == l2p2 = Just l1p1
  | l1p2 == l2p1 = Just l1p2
  | l1p2 == l2p2 = Just l1p2
  | otherwise = Nothing

commonPointExistFor :: LineSegment' -> LineSegment' -> Bool
commonPointExistFor l1 l2 = (maybeCommonPointFor l1 l2) /= Nothing

intersectionPointFor :: LineSegment' -> LineSegment' -> Maybe Point'
intersectionPointFor l1 l2
  | commonPointExistFor l1 l2 = maybeCommonPointFor l1 l2
  | (aFor l1) == (aFor l2) = Nothing
  | otherwise = Just (Point x y)
    where
      aForL1 = aFor l1
      bForL1 = bFor l1
      x = ((bFor l2) - bForL1) / (aForL1 - (aFor l2))
      y = aForL1 * x + bForL1

maybePointIsInsideRectangle :: Maybe Point' -> Bool
maybePointIsInsideRectangle Nothing = False
maybePointIsInsideRectangle (Just intersection) = isPointInsideRectangle intersection

lineSegmentsIntersect :: LineSegment' -> LineSegment' -> Bool
lineSegmentsIntersect l1 l2 = maybePointIsInsideRectangle $ intersectionPointFor l1 l2

rotatePoint :: Point' -> Point'
rotatePoint (Point x y) = Point (x * cos angle - y * sin angle) (x * sin angle + y * cos angle) where
  angle = -pi / 2

rotate :: LineSegment' -> LineSegment'
rotate (LineSegment p1 p2) = LineSegment (rotatePoint p1) (rotatePoint p2)

main = do
  putStrLn $ show (intersectionPointFor (LineSegment topRight bottomLeft) (LineSegment topLeft bottomRight))
