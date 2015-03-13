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
    getElement,
    scatterMatrix
) where

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
                            sampler = sample (acceptanceRatio q)
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

scatterMatrix :: X -> Matrix Double
scatterMatrix x = foldl (+) (zeros d d) $ map (\ p -> (p-xm)<>(trans (p-xm)) ) matrixX
                    where n = fromIntegral $ length x :: Double
                          d = fromIntegral $ dim $ head x
                          xm = fromRows $ [ fst $ meanCov $ fromRows x ]
                          matrixX = map (\p -> fromRows [p]) x


lnormalInvWishart :: X -> Double
lnormalInvWishart x = - (n * d * 2 * (log pi))
                      + (mlgamma d (vn/2))
                      - (mlgamma d (v0/2))
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   n = fromIntegral $ length x :: Double
                            d = fromIntegral $ dim $ head x
                            v0 = 100 -- hyperparameter must be (v0 > d)
                            vn = v0 + n
                            alpha0 = (ident $ round d) -- hyperparameter for the shape of the covariance matrix
                            alphaN = alpha0 + s + (scale ((k0*n)/(k0+kn)) ((xm-mu0)<>(trans (xm-mu0))))
                            ldalpha0 = absLnDet * signDet
                                     where (_,(absLnDet,signDet)) = invlndet alpha0
                            ldalphaN = absLnDet * signDet
                                     where (_,(absLnDet,signDet)) = invlndet alphaN
                            k0 = 0.2 :: Double-- hyperparameter
                            kn = k0 + n
                            s = scatterMatrix x
                            mu0 = fromRows [head $ toRows $ zeros 1 (round d)] -- hyperparameter
                            xm = fromRows $ [ fst $ meanCov $ fromRows x ]



lnormalPrior :: X -> Double
lnormalPrior x = -(n/2)*lDetSigma -(1/2)* tr (invSigma <> s)
                where n = fromIntegral $ length x
                      d = dim $ head x
                      tr m = sum $ toList $ takeDiag m
                      s = scatterMatrix x
                      (invSigma,(absLSigma,signLSigma)) = invlndet sigma
                      lDetSigma = absLSigma * signLSigma
                      sigma = ident $ d -- Hyperparameter for the covariance of the cluster


cofG :: [Double]
cofG = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]

serG :: Double
serG = 1.000000000190015

lgamma :: Double -> Double
lgamma xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                ser' = serG + (sum $ zipWith (/) cofG [xx+1..])
            in -tmp' + log(2.5066282746310005 * ser' / xx)

mlgamma :: Double -> Double -> Double
mlgamma p a = (log pi)*(p*(p-1)/4) + sum (map (\j -> lgamma (a + ((1-j)/2))) [1..p])

