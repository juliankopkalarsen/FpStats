{-|
Module      : Test
Description : Test module running the GMM
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

-}
module Main (
    main
) where

import Test.Framework (defaultMain)

import Math.Test
import MCMC.Test

main :: IO ()
main = defaultMain [mathSuite, mcmcSuite]
