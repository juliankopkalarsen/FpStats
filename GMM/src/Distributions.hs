{-|
Module      : Distributions
Description : Definition of the Distributions used in Models and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental
-}
module Distributions (
    lnormalInvWishart,
    lnormalInvWishartSS,
    lMixNormWish,
    delta,
    Expr,
    dlNormW,
    lNormW
) where

import Math (X,
             mlgamma,
             scatterMatrix,
             meanv,
             lnDeterminant)

import Numeric.LinearAlgebra (Matrix,
                              Vector,
                              (<>),
                              ident,
                              dim,
                              scale,
                              trans,
                              takeDiag,
                              outer)

import Numeric.LinearAlgebra.Data (konst, size)

import Partition (Partition, Component, groups, group)

import LikSpecLang

-- | Delta log Normal-Wishard likelihood for a mixture of components.
dlNormW :: X -> (Partition -> Partition -> Double)
dlNormW x = delta (lMixNormWish x)

-- | Log Normal-Wishard likelihood for a mixture of components.
lNormW :: X -> (Partition -> Double)
lNormW x = compile (lMixNormWish x)

-- | Log Normal-Wishard likelihood for a mixture of components. Same as 'lNormW' but defined without the "LikSpecLang" (LSL)
lMixNormWish ::X -> Expr Partition Double
lMixNormWish x = Mixture (Fun lnormalInvWishart) % selection x -- The '%' operator "pipes" expressions together

selection :: X -> Expr Partition [X]
selection x = Fun (groups x)


-- | Log Normal-Wishard likelihood for a single component
lnormalInvWishartSS :: Int -> Matrix Double -> Vector Double-> Double
lnormalInvWishartSS n s mu = - fromIntegral n * d * 2 * log pi
                      + mlgamma d (vn/2)
                      - mlgamma d (v0/2)
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   d = fromIntegral $ fst $ size s :: Double
                            v0 = 100 -- hyperparameter must be (v0 > d)
                            vn = v0 + fromIntegral n
                            alpha0 = ident $ fst $ size s -- hyperparameter for the shape of the covariance matrix
                            alphaN = alpha0 + s + scale ((k0*fromIntegral n)/(k0+kn)) ((mu-mu0) `outer` (mu-mu0))
                            ldalpha0 = lnDeterminant alpha0
                            ldalphaN = lnDeterminant alphaN
                            k0 = 0.2 -- hyperparameter
                            kn = k0 + fromIntegral n
                            mu0 = (konst 0.0 $ fst $ size s ):: Vector Double -- hyperparameter



-- | Log Normal-Wishard likelihood for a single component
lnormalInvWishart :: X -> Double
lnormalInvWishart x = - n * d * 2 * log pi
                      + mlgamma d (vn/2)
                      - mlgamma d (v0/2)
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   n = fromIntegral $ length x
                            d = fromIntegral $ dim $ head x
                            v0 = 100 -- hyperparameter must be (v0 > d)
                            vn = v0 + n
                            alpha0 = ident $ dim $ head x -- hyperparameter for the shape of the covariance matrix
                            alphaN = alpha0 + s + scale ((k0*n)/(k0+kn)) ((xm-mu0) `outer` (xm-mu0))
                            ldalpha0 = lnDeterminant alpha0
                            ldalphaN = lnDeterminant alphaN
                            k0 = 0.2 -- hyperparameter
                            kn = k0 + n
                            s = scatterMatrix x
                            mu0 = (konst 0.0 $ dim $ head x ):: Vector Double -- hyperparameter
                            xm = meanv x
