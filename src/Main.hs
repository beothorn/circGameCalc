module Main (main) where

--------------------------------------------------------
-- All non intersecting, non rotated lines for points --
--                                                    --
--                    1,3 2,3                         --
--             0,3 .___x___x___                       --
--                 |           |                      --
--             0,2 x           x 3,2                  --
--                 |           |                      --
--             0,1 x           x 3,1                  --
--                 |___x___x___|.3,0                  --
--                    1,0 2,0                         --
--------------------------------------------------------

data Point' = Point {x :: Float, y :: Float} deriving (Show, Eq)

topLeft =     Point 1.0 3.0
topRight =    Point 2.0 3.0
rightTop =    Point 3.0 2.0
rightBottom = Point 3.0 1.0
bottomRight = Point 2.0 0.0
bottomLeft =  Point 1.0 0.0
leftBottom =  Point 0.0 1.0
leftTop =     Point 0.0 2.0

thePoints :: [Point']
thePoints = [topRight, topLeft, rightTop, rightBottom, bottomRight, bottomLeft, leftBottom, leftTop]

data LineSegment' = LineSegment {start :: Point', end :: Point'} deriving (Show, Eq)

allPossibleLinesFor :: [Point'] -> [LineSegment']
allPossibleLinesFor [] = []
allPossibleLinesFor (p:pointsLeft) = allLinesForFirstPoint ++ allPossibleLinesFor pointsLeft where
 allLinesForFirstPoint = map (\otherPoint -> LineSegment p otherPoint) pointsLeft

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

main = do
  putStrLn $ show (intersectionPointFor (LineSegment topRight bottomLeft) (LineSegment topLeft bottomRight))
