-----------------------------------------------------------------------------
--
-- Module      :  Math
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

module Math (
    mlgamma,
    scatterMatrix
) where

import Numeric.LinearAlgebra (Matrix, Vector, trans, (<>), fromRows, dim, meanCov)
import Numeric.LinearAlgebra.Util  (zeros)

type X = [Vector Double]

scatterMatrix :: X -> Matrix Double
scatterMatrix x = foldl (+) (zeros d d) $ map (\ p -> (p-xm)<>(trans (p-xm)) ) matrixX
                    where n = fromIntegral $ length x :: Double
                          d = fromIntegral $ dim $ head x
                          xm = fromRows $ [ fst $ meanCov $ fromRows x ]
                          matrixX = map (\p -> fromRows [p]) x

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

