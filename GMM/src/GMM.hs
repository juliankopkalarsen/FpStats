{-|
Module      : GMM
Description : Definition of the Gaussian Mixture Model and Metropolis hatings sampler.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental


-}
module GMM (
    X,
    getElement
) where


import GHC.Arr (range)
import Data.List (foldl')
import Data.Array
import System.Random
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util hiding ((!))
import Debug.Trace

import Distributions -- (lnormalInvWishart, lMixNormWish, delta, Expr)
import Partition (Partition, Component, Move, naivefromNk, genMoves, applyMove, group)

import Math (X)

type Chain = [Partition]

type Likelihood = (X -> Double)

type Proposal = (Move, Double)

props :: Int -> Int -> Int -> [Proposal]
props seed n k = zip moves rand_accepts
                where rand_accepts = randomRs (0.0,1.0) (mkStdGen seed) :: [Double]
                      moves = genMoves seed n k

sample :: (Partition -> Move -> Double) -> Partition -> Proposal -> Partition
sample f prev_state (m, accept_prop)
    | accept_prop < f prev_state m = new_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Accepted: " ++ show (f a m)) b
    | otherwise                    = prev_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Rejected: " ++ show (f a m)) a
                                    where new_state = applyMove prev_state m

getElement :: X -> Int -> Int -> Int -> Partition
getElement x seed k nSamples = foldl' sampler start $ take nSamples (props seed n k)
                      where n = length x
                            start = naivefromNk n k
                            sampler = sample $ acceptanceRatio dq
                            dq = dlNormW x

acceptanceRatio :: (Partition -> Partition -> Double) -> Partition -> Move -> Double
acceptanceRatio dq x m = exp (dq x' x)
                        where x' = applyMove x m






