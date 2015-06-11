
{-|
Module      : MCMC
Description : Definition of the Marcov Chain Monte Carlo samplers ie. the Metropolis hatings sampler.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental


-}-------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module MCMC (
    Moveable(move),
    Sampleable(condMove, llikelihood, llikDiff),
    getMHChain,
    getElement
) where

import System.Random (RandomGen, split, randomR)
import Data.List (foldl')


class Moveable m where
    move :: (RandomGen r) =>  r -> m -> m

class Sampleable a d where
    condMove ::(RandomGen r) => d -> r -> a -> a
    llikelihood :: d -> a -> Double
    llikDiff :: d -> (a, a) -> Double
    -- llikDiff d a a' = (llikelihood d a) - (llikelihood d a')

unitSample :: (RandomGen r)=> r -> Double
unitSample g = fst $ randomR (0.0, 1.0) g

sample :: (RandomGen r, Sampleable a d) => d -> a -> r -> a
sample x prev_state gen
    | unitSample sampleGen < alpha  = new_state  -- trace ("accepted, llik: " ++ show llik ++ "a: " ++ show alpha) new_state
    | otherwise                     = prev_state -- trace ("rejected, llik: " ++ show llik ++ "a: " ++ show alpha) prev_state
                                where alpha = exp $ llikDiff x (prev_state, new_state)
                                      new_state = condMove x moveGen prev_state
                                      (moveGen, sampleGen) = split gen
                                      llik = llikelihood x new_state

getMHChain :: (RandomGen r, Sampleable a d) => r -> d -> a -> [a]
getMHChain gen x start = start:getMHChain aGen x (sample x start bGen)
                     where (aGen, bGen) = split gen

getElement :: (RandomGen r, Sampleable a d) => r -> d -> a -> Int -> a
getElement gen x start n = foldl' sample_parameters start (take n $ randoms gen)
            where sample_parameters = sample x

randoms:: (RandomGen r)=> r-> [r]
randoms g = a:(randoms b)
            where (a,b) = split g
