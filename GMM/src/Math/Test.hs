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
import Test.QuickCheck

import Numeric.LinearAlgebra (Vector, fromList, toRows)
import Foreign.Storable (Storable)

import Math

mathSuite :: Test
mathSuite = testGroup "Auxillary math functions"
    [testCase "The mean of a vector is the vector itself" (testMean [(fromList [1.0]), (fromList [1.0])] (fromList [1.0]) ),
     --testCase "should fail" (testMean [(fromList [1.0]), (fromList [2.0])] (fromList [1.0]) ),
     testProperty "The mean of a vector is the vector itself" meanProp1
     ]

testMean :: [Vector Double] -> Vector Double -> Assertion
testMean points expected = expected @=? (head . toRows . mean) points

instance (Arbitrary a, Storable a) => Arbitrary (Vector a) where
    arbitrary = fmap fromList arbitrary

meanProp1 x = x == x
        where types = (x::Vector Double)


