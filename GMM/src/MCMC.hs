{-|
Module      : MCMC
Description : Definition of the Marcov Chain Monte Carlo samplers ie. the Metropolis hatings sampler.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental


-}-------------------------------------------------------------------------

module MCMC (

) where

import System.Random

class Moveable m where
    move :: (RandomGen r) => r -> m -> m

class LikRatioable m where
    likRatio :: (Ord a)=> m -> m -> a

--getElement
--getElement x seed k nSamples = foldl' sampler start $ take nSamples (props seed n k)
--                      where n = length x
--                            start = naivefromNk n k
--                            sampler = sample $ acceptanceRatio dq
--                            dq = dlNormW x

unitSample :: (RandomGen r)=> r -> Double
unitSample g = fst $ randomR (0.0, 1.0) g

sample :: (RandomGen r, Moveable a, LikRatioable a) => r -> a -> a
sample gen prev_state
    | unitSample genSample < alpha  = new_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Accepted: " ++ show (f a m)) b
    | otherwise                 = prev_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Rejected: " ++ show (f a m)) a
                                where alpha = likRatio prev_state new_state
                                      new_state = move genMove prev_state
                                      (genMove, genSample) = split gen

getMHChain :: (RandomGen r, Moveable a, LikRatioable a) => r -> a -> [a]
getMHChain gen start = start:getMHChain  ra (sample rb start)
                     where (ra, rb) = split gen
