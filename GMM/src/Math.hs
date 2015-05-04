{-|
Module      : Math
Description : Auxiliary math functions
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Definition of math functions like 'scatterMatrix' and the multivariate log Gamma function 'mlgamma'
-}
module Math (
    X,
    mlgamma,
    scatterMatrix,
    mean
) where

import Numeric.LinearAlgebra (Matrix, Vector, trans, (<>), fromRows, dim, meanCov)

-- | Type synonym for data objects that can be anything as long as it is compatible with the likelihood
--   could be extended to a type class with sufficient statistics member functions.
type X = [Vector Double]

-- | Computes the scatter matrix defined as:
scatterMatrix :: X -> Matrix Double
scatterMatrix x = sum $ map (\ p -> (p-xm)<>trans (p-xm) ) matrixX
                    where n = fromIntegral $ length x :: Double
                          d = fromIntegral $ dim $ head x
                          xm = mean x
                          matrixX = map (\p -> fromRows [p]) x

-- | Mean of a dataset
mean :: [Vector Double] -> Matrix Double
mean x = fromRows [ fst $ meanCov $ fromRows x ]


cofG :: [Double]
cofG = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]

serG :: Double
serG = 1.000000000190015

lgamma :: Double -> Double
lgamma xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                ser' = serG + (sum $ zipWith (/) cofG [xx+1..])
            in -tmp' + log(2.5066282746310005 * ser' / xx)

mlgamma :: Double -> Double -> Double
mlgamma p a = (log pi)*(p*(p-1)/4) + sum (map (\j -> lgamma (a + ((1-j)/2))) [1..p])

