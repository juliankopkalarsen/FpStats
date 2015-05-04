-----------------------------------------------------------------------------
--
-- Module      :  Math.Test
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Math.Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test)
import Numeric.LinearAlgebra (Vector, fromList, toRows)

import Math

mathSuite :: Test
mathSuite = testGroup "Auxillary math functions"
    [testCase "The mean of a vector is the vector itself" (testMean [(fromList [1.0]), (fromList [1.0])] (fromList [1.0]) ),
     --testCase "should fail" (testMean [(fromList [1.0]), (fromList [2.0])] (fromList [1.0]) ),
     testCase "The mean of a longer vector is the vector itself" (testMean [(fromList [1.0, 2.0]), (fromList [1.0, 2.0])] (fromList [1.0, 2.0]) )
     ]

testMean :: [Vector Double] -> Vector Double -> Assertion
testMean points expected = expected @=? (head . toRows . mean) points
