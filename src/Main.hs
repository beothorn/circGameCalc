module Main (main) where

--------------------------------------------------------
-- All non intersecting, non rotated lines for points --
--                                                    --
--                    1,3 2,3                         --
--                  ___x___x___                       --
--                 |           |                      --
--             0,2 x           x 3,2                  --
--                 |           |                      --
--             0,1 x           x 3,1                  --
--                 |___x___x___|                      --
--                    1,0 2,0                         --
--------------------------------------------------------

data Point' = Point Int Int

topRight = Point 1 3
topLeft = Point 2 3
rightTop = Point 3 2
rightBottom = Point 3 1
bottomRight = Point 2 0
bottomLeft = Point 1 0
leftBottom = Point 0 1
leftTop = Point 0 2

thePoints :: [Point']
thePoints = [topRight, topLeft, rightTop, rightBottom, bottomRight, bottomLeft, leftBottom, leftTop]

data LineSegment' = LineSegment Point' Point'

aLine = LineSegment topRight bottomRight
anotherLine = LineSegment leftTop rightTop

pointToString :: Point'->String
pointToString  (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

lineToString :: LineSegment'->String
lineToString (LineSegment start end) = "[" ++ pointToString start ++ "->" ++ pointToString end ++ "]"

linesToString :: [LineSegment'] -> String
linesToString lines = foldr (++) "" (map lineToString lines )

allPossibleLines :: [Point'] -> [LineSegment']
allPossibleLines [] = []
allPossibleLines (p:pTail) = (map (\otherPoint -> LineSegment p otherPoint) pTail) ++ allPossibleLines pTail

calc::String
calc= linesToString $ allPossibleLines thePoints

main = do
  putStrLn calc
  putStrLn $ show (length (allPossibleLines thePoints))
