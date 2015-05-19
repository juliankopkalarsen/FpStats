{-# LANGUAGE FlexibleInstances #-}
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
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.AEq (AEq,(~==))

import Numeric.LinearAlgebra (Matrix, Vector, Element, fromList, toRows, toList, flatten)
import Foreign.Storable (Storable)

import Math

mathSuite :: Test
mathSuite = testGroup "Auxillary math functions"
    [-- testCase "The mean of a vector is the vector itself" (testMean [(fromList [1.0]), (fromList [1.0])] (fromList [1.0]) ),
     -- testCase "should fail" (testMean [(fromList [1.0]), (fromList [2.0])] (fromList [1.0]) ),
     testProperty "The mean of a vector is the vector itself" meanProp1,
     testProperty "Mean addition" mean_addition,
     testProperty "Alternate ScatterMatrix" scatter_alternative,
     testProperty "Mean subtraction" mean_subtraction,
     testProperty "ScatterMatrix addition" scatter_addition,
     testProperty "ScatterMatrix subtraction" scatter_subtraction,
     testProperty "The addition of Summarystats" sstat_addition,
     testProperty "The subtraction of Summarystats" sstat_subtraction
     ]

testMean :: [Vector Double] -> Vector Double -> Assertion
testMean points expected = expected @=? meanv points

instance Arbitrary (Vector Double) where
    arbitrary = sized $ \n -> resize (n+1) $ fmap fromList arbitrary

instance Arbitrary X where
    arbitrary = sized $ \n -> sequence [fmap fromList (vector 3) | i<-[1..n]]

instance (AEq a, Storable a)=> AEq (Vector a) where
    x ~== y = (toList x) ~== (toList y)

instance AEq (Matrix Double) where
    x ~== y = (flatten x) ~== (flatten y)

scatter_alternative x = length x > 0 ==> scatterMatrix x ~== scatterMatrix_alt x
            where types = (x::X)

meanProp1 x = meanv [x] == x
        where types = (x::Vector Double)

mean_addition x k = k <= length x ==> add_means nx (meanv xs) ny (meanv ys) ~== meanv (xs++ys)
            where nx = length xs
                  ny = length ys
                  xs = take k x
                  ys = drop k x
                  types = (x::X, k::Int)

mean_subtraction x k = k <= length x ==> sub_means (nx+ny) (meanv (xs++ys)) ny (meanv ys) ~== meanv xs
            where nx = length xs
                  ny = length ys
                  xs = take k x
                  ys = drop k x
                  types = (x::X, k::Int)

scatter_addition x k = k <= length x ==> add_scatter nx sx mx ny sy my ~== scatterMatrix x
            where nx = length xs
                  ny = length ys
                  xs = take k x
                  ys = drop k x
                  mx = meanv xs
                  my = meanv ys
                  sx = scatterMatrix xs
                  sy = scatterMatrix ys
                  types = (x::X, k::Int)

scatter_subtraction x k = k>= 1 && k >= (quot (length x) 2)+1 && k <= length x ==> sub_scatter n s m ny sy my ~== scatterMatrix xs
            where xs = take k x
                  ys = drop k x
                  n = length x
                  ny = length ys
                  m = meanv x
                  my = meanv ys
                  s = scatterMatrix x
                  sy = scatterMatrix ys
                  types = (x::X, k::Int)


sstat_addition x k = k <= length x ==> (fromX xs) <+> (fromX ys) ~== (fromX x)
            where   xs = take k x
                    ys = drop k x
                    types = (x::X, k::Int)

sstat_subtraction x k = k < length x ==> (fromX x) <-> (fromX xs) ~== (fromX ys)
            where   xs = take k x
                    ys = drop k x
                    types = (x::X, k::Int)

validX xs ys = length xs > 1
               && length ys > 1
               && dimension xs > 1
               && dimension ys > 1
               && dimension xs == dimension ys


