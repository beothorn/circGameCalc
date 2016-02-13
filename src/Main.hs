module Main (main) where

import Calc.PieceLogic

main = do
  putStrLn $ show $ onlyUniquePieces $ makeAPieceForEachLineSegment $ allPossibleLinesFor thePoints
