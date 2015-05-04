{-|
Module      : Distributions
Description : Definition of the Distributions used in Models and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental
-}
module Distributions (
    lnormalInvWishart,
    lMixNormWish,
    delta,
    Expr,
    dlNormW,
    lNormW
) where

import Math (X,
             mlgamma,
             scatterMatrix,
             mean)

import Numeric.LinearAlgebra (Vector,
                              (<>),
                              invlndet,
                              ident,
                              dim,
                              scale,
                              trans,
                              takeDiag)

import Numeric.LinearAlgebra.Util  (zeros)

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
lMixNormWish x = Mixture (Fun (lnormalInvWishart . group x))
-- lMixNormWish x p = sum $ map lnormalInvWishart (groups x p )

lnormw :: Expr X Double
lnormw = Fun lnormalInvWishart

-- | Log Normal-Wishard likelihood for a single component
lnormalInvWishart :: X -> Double
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
                            mu0 = zeros 1 (round d) -- hyperparameter
                            xm = mean x
