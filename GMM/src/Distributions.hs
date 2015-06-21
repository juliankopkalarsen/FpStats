{-|
Module      : Distributions
Description : Definition of the Distributions used in Models and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental
-}
module Distributions (
    lnormalInvWishart,
    lnormalInvWishartSS
) where

import Math (X,
             Sstat,
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


-- | Log Normal-Wishard likelihood for a single component
lnormalInvWishartSS :: Sstat -> Double
lnormalInvWishartSS (n, mu, s) = - fromIntegral n * d * 2 * log pi
                      + mlgamma d (vn/2)
                      - mlgamma d (v0/2)
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   d = fromIntegral $ fst $ size s :: Double
                            v0 = 100 -- hyperparameter must be (v0 > d)
                            vn = v0 + fromIntegral n
                            t0 = ident $ fst $ size s -- hyperparameter for the shape of the covariance matrix
                            tN = t0 + s + scale ((k0*fromIntegral n)/(k0+kn)) ((mu-mu0) `outer` (mu-mu0))
                            ldalpha0 = lnDeterminant t0
                            ldalphaN = lnDeterminant tN
                            k0 = 0.2 -- hyperparameter
                            kn = k0 + fromIntegral n
                            mu0 = (konst 0.0 $ fst $ size s ):: Vector Double -- hyperparameter



-- | Log Normal-Wishard likelihood for a single component
lnormalInvWishart :: X -> Double
lnormalInvWishart [] = 0
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
