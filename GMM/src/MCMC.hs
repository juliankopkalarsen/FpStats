
{-|
Module      : MCMC
Description : Definition of the Marcov Chain Monte Carlo samplers ie. the Metropolis hatings sampler.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental


-}-------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module MCMC (
    Moveable(move),
    Sampleable(propMove, evalLikelihoodRatio),
    getMHChain,
    getElement
) where

import System.Random (RandomGen, split, randomR)
import Data.List (foldl')

class Moveable m where
    move :: (RandomGen r) =>  r -> m -> m

class Sampleable parameter context where
    propMove ::(RandomGen rng) => context -> rng -> parameter -> parameter
    evalLikelihoodRatio :: context -> parameter -> parameter -> Double

unitSample :: (RandomGen r)=> r -> Double
unitSample g = fst $ randomR (0.0, 1.0) g

sample :: (RandomGen rng, Sampleable parameter context) => context -> parameter -> rng -> parameter
sample x prev_state gen
    | unitSample sampleGen < alpha  = new_state -- trace ("accepted, llik: " ++ show llik ++ "a: " ++ show alpha) new_state
    | otherwise                     = prev_state -- trace ("rejected, llik: " ++ show llik ++ "a: " ++ show alpha) prev_state
                                where alpha = evalLikelihoodRatio x new_state prev_state
                                      new_state = propMove x moveGen prev_state
                                      (moveGen, sampleGen) = split gen

getMHChain :: (RandomGen rng, Sampleable a d) => rng-> d -> a -> [a]
getMHChain gen x start = start:getMHChain aGen x (sample x start bGen)
                     where (aGen, bGen) = split gen

getElement :: (RandomGen rng, Sampleable a d) => rng-> d -> a -> Int -> a
getElement gen x start n = foldl' sample_parameters start (take n $ randoms gen)
            where sample_parameters = sample x

randoms:: (RandomGen rng)=> rng-> [rng]
randoms g = a:(randoms b)
            where (a,b) = split g
