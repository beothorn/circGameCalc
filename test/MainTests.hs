module Main where

import QuickCheck.PieceLogicTests
import Test.Framework (defaultMain)

main =  defaultMain [qcPieceLogicSuite]
