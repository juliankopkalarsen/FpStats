-----------------------------------------------------------------------------
--
-- Module      :  GMM
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

module GMM (
    X,
    Partition,
    getElement,
    select,
    numComponents
) where

import Distributions (lnormalInvWishart)

import System.Random
import GHC.Arr (range)
import Data.List
import Data.Array
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util hiding ((!))
import Debug.Trace

import Data.MemoTrie

-- Data can be anything as long as it is compatible with the likelihood
type X = [Vector Double]

type Chain = [Partition]

type Partition = [[Int]]

type Likelihood = (X -> Double)

naivefromXNk :: Int -> Int -> Partition
naivefromXNk n k = transpose $ splitEvery k [0..n-1]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where (first,rest) = splitAt n list

blockFromNk :: Int -> Int -> Partition
blockFromNk n k = fst s : snd s : []
                  where n1 = n `div` k
                        s = splitAt n1 [0..n-1]

data Move = Move {   node :: Int
                    ,comp :: Int
                    } deriving Show

apply_move :: Partition -> Move -> Partition
apply_move p m = moveNode c i p
                 where  i = node m
                        c = comp m

moveNode:: Int -> Int -> [[Int]] -> [[Int]]
moveNode 0 i (x:xs)
    | elem i x = x:xs
    | notElem i x = (i:x):(map (delete i) xs)
moveNode n i (x:xs) = (delete i x):(moveNode (n-1) i xs)


applyTo :: (a -> a) -> Int -> [a] -> [a]
applyTo f i l = (take (i-1) l) ++ (f (l!!i)):(drop i l)

genMoves :: Int -> Int -> Int -> [Move]
genMoves seed n k = zipWith (\i c -> Move {node=i, comp=c})
                            (randomRs (0,n-1) (mkStdGen seed))
                            (randomRs (0,k-1) (mkStdGen seed))

sample :: (Partition -> Move -> Double) -> Partition -> Proposal -> Partition
sample f prev_state (m, accept_prop)
    | accept_prop < f prev_state m = new_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Accepted: " ++ show (f a m)) b
    | otherwise                    = prev_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Rejected: " ++ show (f a m)) a
                                    where new_state = apply_move prev_state m

getElement :: X -> Int -> Int -> Int -> Partition
getElement x seed k nSamples = foldl' sampler start $ take nSamples (props seed n k)
                      where n = length x
                            start = naivefromXNk n k
                            sampler = sample $ acceptanceRatio q
                            q = sum . map (lik)
                            lik = lnormalInvWishart . map (x!!)


type Proposal = (Move, Double)

props :: Int -> Int -> Int -> [Proposal]
props seed n k = zip moves rand_accepts
                where rand_accepts = randomRs (0.0,1.0) (mkStdGen seed) :: [Double]
                      moves = genMoves seed n k

select :: X -> Partition -> Int -> X
select x p i = map (x!!) (p!!i)

numComponents :: Partition -> Int
numComponents = length

acceptanceRatio :: (Partition -> Double) -> Partition -> Move -> Double
acceptanceRatio q x m = logDiffRatio (q x') (q x)
                        where x' = apply_move x m

-- | Calculates the ratio between two likelihoods
--   given the logs of them.
logDiffRatio :: Double -> Double -> Double
logDiffRatio a b = exp (a - b)

qtrace x = trace ("value: " ++ show x) x



