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


fac :: Integer -> Integer
fac n = product [1..n]

--allPossibleLinesFor :: [Point'] -> [LineSegment']
prop_allLinesSize :: [Point'] -> Bool
prop_allLinesSize [] = True
prop_allLinesSize (x: []) = True
prop_allLinesSize points = toInteger (length (allPossibleLinesFor points)) == toInteger ((fromIntegral $ fac n) `div` ((fromIntegral $ fac p) * (fromIntegral $ fac (n-p)))) where
  p = 2::Integer
  n = toInteger (length points)

qcPieceLogicSuite :: Test
qcPieceLogicSuite = testGroup "QuickCheckPieceLogic"
  [ testProperty "Will have the expected ammount for a permutation" prop_allLinesSize ]
