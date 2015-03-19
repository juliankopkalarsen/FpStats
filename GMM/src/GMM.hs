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
    getElement
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

type Partition = [Int]

type Likelihood = (X -> Double)

naivefromXNk :: Int -> Int -> Partition
naivefromXNk n k = take n $ cycle [1..k]

blockFromNk :: Int -> Int -> Partition
blockFromNk n k = (replicate n1 1)++(replicate (n-n1) 2)
                    where n1 = 100

data Move = Move {   node :: Int
                    ,comp :: Int
                    } deriving Show

apply_move :: Partition -> Move -> Partition
apply_move p m = a ++ (c:b)
                    where   (a,b) = case splitAt i p of
                                    (xp,_:xs) -> (xp,xs)
                                    (xp,[]) -> (xp,[])
                            i = node m
                            c = comp m

genMoves :: Int -> Int -> Int -> [Move]
genMoves seed n k = zipWith (\i c -> Move {node=i, comp=c})
                            (randomRs (1,n) (mkStdGen seed))
                            (randomRs (1,k) (mkStdGen seed))

sample :: (Partition -> Move -> Double) -> Partition -> Proposal -> Partition
sample f prev_state (m, accept_prop)
    | accept_prop < f prev_state m = new_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Accepted: " ++ show (f a m)) b
    | otherwise                    = prev_state -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Rejected: " ++ show (f a m)) a
                                    where new_state = apply_move prev_state m

getElement :: X -> Int -> Int -> Int -> Partition
getElement x seed k nSamples = foldl' sampler start $ take nSamples (props seed n k)
                      where n = length x
                            start = naivefromXNk n k
                            sampler = sample $ acceptanceRatio $ memo q
                            q param = sum . map lnormalInvWishart $ split_data x param


type Proposal = (Move, Double)

props :: Int -> Int -> Int -> [Proposal]
props seed n k = zip moves rand_accepts
                where rand_accepts = randomRs (0.0,1.0) (mkStdGen seed) :: [Double]
                      moves = genMoves seed n k

select :: X -> Partition -> Int -> X
select x p i =  fst . unzip $ filter (\(_,c)->c==i) $ zip x p

split_data :: X -> Partition -> [X]
split_data x p = map (select x p) [1..k]
                 where   k = maximum p

acceptanceRatio :: (Partition -> Double) -> Partition -> Move -> Double
acceptanceRatio q x m = logDiffRatio (q x') (q x)
                        where x' = apply_move x m

-- | Calculates the ratio between two likelihoods
--   given the logs of them.
logDiffRatio :: Double -> Double -> Double
logDiffRatio a b = exp (a - b)

qtrace x = trace ("value: " ++ show x) x



