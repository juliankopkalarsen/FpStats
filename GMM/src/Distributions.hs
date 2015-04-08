-----------------------------------------------------------------------------
--
-- Module      :  Distributions
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

module Distributions (
    lnormalInvWishart,
    lMixNormWish,
    delta,
    Expr,
    dlNormW,
    lnormalPrior
) where

import Math (mlgamma,
             scatterMatrix)

import Numeric.LinearAlgebra (Vector,
                              toList,
                              (<>),
                              invlndet,
                              ident,
                              dim,
                              fromRows,
                              scale,
                              trans,
                              toRows,
                              meanCov,
                              takeDiag)

import Numeric.LinearAlgebra.Util  (zeros)

import Partition (Partition, Component, groups)

type X = [Vector Double]

data Expr i o = Fun (i -> o)

type DeltaLik = Partition -> Partition -> Double

delta :: Expr Partition Double -> DeltaLik
delta (Fun f) = \x' -> \x -> (f x') - (f x)

dlNormW :: X -> DeltaLik
dlNormW x = delta (Fun (lMixNormWish x))


lMixNormWish :: X -> Partition -> Double
lMixNormWish x p = sum $ map lnormalInvWishart (groups x p )

--lnormalInvWishart :: X -> Double
lnormalInvWishart x = - n * d * 2 * log pi
                      + mlgamma d (vn/2)
                      - mlgamma d (v0/2)
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   n = fromIntegral $ length x :: Double
                            d = fromIntegral $ dim $ head x
                            v0 = 100 -- hyperparameter must be (v0 > d)
                            vn = v0 + n
                            alpha0 = ident $ round d -- hyperparameter for the shape of the covariance matrix
                            alphaN = alpha0 + s + scale ((k0*n)/(k0+kn)) (xm-mu0)<> trans (xm-mu0)
                            ldalpha0 = absLnDet * signDet
                                     where (_,(absLnDet,signDet)) = invlndet alpha0
                            ldalphaN = absLnDet * signDet
                                     where (_,(absLnDet,signDet)) = invlndet alphaN
                            k0 = 0.2 :: Double-- hyperparameter
                            kn = k0 + n
                            s = scatterMatrix x
                            mu0 = fromRows [head $ toRows $ zeros 1 (round d)] -- hyperparameter
                            xm = fromRows [ fst $ meanCov $ fromRows x ]



lnormalPrior :: X -> Double
lnormalPrior x = -(n/2)*lDetSigma -(1/2)* tr (invSigma <> s)
                where n = fromIntegral $ length x
                      d = dim $ head x
                      tr m = sum $ toList $ takeDiag m
                      s = scatterMatrix x
                      (invSigma,(absLSigma,signLSigma)) = invlndet sigma
                      lDetSigma = absLSigma * signLSigma
                      sigma = ident d -- Hyperparameter for the covariance of the cluster


