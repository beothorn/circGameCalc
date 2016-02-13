module QuickCheck.PieceLogicTests where

import Calc.PieceLogic
import Test.QuickCheck

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Point' where
   arbitrary = do
     Blind x <- arbitrary
     Blind y <- arbitrary
     return $ Point x y

fac n = product [1..n]

--allPossibleLinesFor :: [Point'] -> [LineSegment']
prop_allLinesSize :: [Point'] -> Bool
prop_allLinesSize [] = True
prop_allLinesSize (x: []) = True
prop_allLinesSize points = length (allPossibleLinesFor points) == (fac p) `div` ((fac n) * (fac (n-p))) where
  p = 2
  n = length points

qcPieceLogicSuite :: Test
qcPieceLogicSuite = testGroup "QuickCheckPieceLogic"
  [ testProperty "Will have the expected ammount for a permutation" prop_allLinesSize ]
