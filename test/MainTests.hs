module Main where

import Calc.PieceLogic
import Test.QuickCheck

instance Arbitrary Point' where
   arbitrary = do
     Blind x <- arbitrary
     Blind y <- arbitrary
     return $ Point x y

fac n = product [1..n]

--allPossibleLinesFor :: [Point'] -> [LineSegment']
prop_allLinesSize :: [Point'] -> Bool
prop_allLinesSize points = length (allPossibleLinesFor points) == (fac p) `div` ((fac n) * (fac (n-p))) where
  p = 2
  n = length points

main = quickCheck prop_allLinesSize
