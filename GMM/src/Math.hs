{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Math
Description : Auxiliary math functions
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Definition of math functions like 'scatterMatrix' and the multivariate log Gamma function 'mlgamma'
-}
{-# LANGUAGE TypeSynonymInstances #-}
module Math (
    X,
    dimension,
    Sstat,
    add_means,
    sub_means,
    add_scatter,
    sub_scatter,
    fromX,
    AbelianGroup((<+>), (<->)),
    mlgamma,
    scatterMatrix,
    scatterMatrixAlt,
    mean,
    meanv,
    lnDeterminant,
    frequency,
    normalizeFrequency,
    normalizeDist
) where

import Numeric.LinearAlgebra (Matrix, Vector, trans, (<>), fromRows, dim, meanCov, invlndet, outer, konst, scale)
import Numeric.LinearAlgebra.Data (size)
import Debug.Trace
import Data.List ((\\), sort, group)
import Control.Arrow ((&&&))

qtr x = trace ("value: " ++ show x) x

-- | Class for the
class AbelianGroup a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a

-- | Type synonym for data objects that can be anything as long as it is compatible with the likelihood
--   could be extended to a type class with sufficient statistics member functions.
type X = [Vector Double]

-- | Computes the scatter matrix defined as:
scatterMatrixAlt :: X -> Matrix Double
scatterMatrixAlt x = sum $ map (\v -> outer (v-xm) (v-xm)) x
                where xm = meanv x

scatterMatrix :: X -> Matrix Double
scatterMatrix x = sum (map (\v -> outer v v) x) - scale n (outer xm xm)
                    where xm = meanv x
                          n = fromIntegral $ length x

-- | Mean of a dataset
mean :: [Vector Double] -> Matrix Double
mean x = fromRows [meanv x]

meanv :: [Vector Double] -> Vector Double
meanv x = scale (1/(fromIntegral $ length x))(sum x)

dimension :: [Vector Double] -> Int
dimension = dim . head

type Sstat = (Int, Matrix Double, Vector Double)

fromX :: X -> Sstat
fromX x = (length x, scatterMatrix x, meanv x)

instance AbelianGroup Sstat where
    s <+> (0,_,_) = s
    (n1, s1, m1) <+> (n2, s2, m2) = (n1+n2, add_scatter n1 s1 m1 n2 s2 m2 , add_means n1 m1 n2 m2)
    s <-> (0,_,_) = s
    (n1, s1, m1) <-> (n2, s2, m2) = (n1-n2, sub_scatter n1 s1 m1 n2 s2 m2, sub_means n1 m1 n2 m2)

instance (AbelianGroup a) => AbelianGroup [a] where
    x <+> y = map (\(a,b) -> a <+> b) $ zip x y
    x <-> y = map (\(a,b) -> a <-> b) $ zip x y

add_means :: Int -> Vector Double -> Int -> Vector Double -> Vector Double
add_means _ m1 0 _ = m1
add_means 0 _ _ m2 = m2
add_means i1 m1 i2 m2 = scale w1 m1 + scale w2 m2
        where n1 = fromIntegral i1
              n2 = fromIntegral i2
              w1 = (n1/(n1+n2))
              w2 = (n2/(n1+n2))

sub_means :: Int -> Vector Double -> Int -> Vector Double -> Vector Double
sub_means _ m1 0 _ = m1
sub_means i1 _ i2 _ | i1==i2 = meanv []
sub_means i1 m1 i2 m2 = scale w1 m1 - scale w2 m2
        where n1 = fromIntegral i1
              n2 = fromIntegral i2
              w1 = (n1/(n1-n2))
              w2 = (n2/(n1-n2))

add_scatter :: Int -> Matrix Double -> Vector Double -> Int -> Matrix Double -> Vector Double -> Matrix Double
add_scatter _ sa _ 0 _ _ = sa
add_scatter 0 _ _ _ sb _ = sb
add_scatter ia sa ma ib sb mb = sa + sb + (scale na xaa) + (scale nb xbb) - (scale n xx)
        where na = fromIntegral ia
              nb = fromIntegral ib
              xaa = outer ma ma
              xbb = outer mb mb
              mx = add_means ia ma ib mb
              xx = outer mx mx
              n = na + nb

sub_scatter :: Int -> Matrix Double -> Vector Double -> Int -> Matrix Double -> Vector Double -> Matrix Double
sub_scatter 0 _ _ n _ _ = error "Nothing to subtract from."
sub_scatter _ sa _ 0 _ _ = sa
sub_scatter ia sa ma ib sb mb | ia==ib = scatterMatrix [ma]
sub_scatter ia sa ma ib sb mb | ia-ib==1 = konst 0 (size sa)
sub_scatter ia sa ma ib sb mb = sa - sb + (scale na xaa) - (scale nb xbb) - (scale n xx)
        where na = fromIntegral ia
              nb = fromIntegral ib
              xaa = outer ma ma
              xbb = outer mb mb
              mx = sub_means ia ma ib mb
              xx = outer mx mx
              n = na - nb


lnDeterminant :: Matrix Double -> Double
lnDeterminant x = absLnDet * signDet
        where (_,(absLnDet,signDet)) = invlndet x


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

frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (\(x:xs) -> (x, length (x:xs))) . group . sort

normalizeFrequency :: (Eq c) => [(c, Int)] -> [(c, Double)]
normalizeFrequency f = map (\(e, i) -> (e, (fromIntegral i)/n)) f
                    where   n = fromIntegral $ sum [i | (e, i)<- f]

normalizeDist :: (Eq c, Fractional a) => [(c, a)] -> [(c, a)]
normalizeDist f = map (\(e,i ) -> (e, i/n)) f
      where   n = sum [i | (e, i)<- f]
