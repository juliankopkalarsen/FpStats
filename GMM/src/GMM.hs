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
    initGChain,
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

-- Data can be anything as long as it is compatible with the likelihood
type X = [Vector Double]

type Chain = [HardAssignment]

type HardAssignment = Array Int Int

naivefromXNk :: Int -> Int -> HardAssignment
naivefromXNk n k = listArray (1,n) $ take n $ cycle $ [1..k]

blockFromNk :: Int -> Int -> HardAssignment
blockFromNk n k = listArray (1,n) $ (replicate n1 1)++(replicate (n-n1) 2)
                    where n1 = 100

data Move = Move {   node :: Int
                    ,comp :: Int
                    } deriving Show

apply_move :: HardAssignment -> Move -> HardAssignment
apply_move a m = a // [(node m, comp m)]

genMoves :: Int -> Int -> Int -> [Move]
genMoves seed n k = zipWith (\i c -> Move {node=i, comp=c})
                            (randomRs (1,n) (mkStdGen seed))
                            (randomRs (1,k) (mkStdGen seed))

sample :: (HardAssignment -> Move -> Double) -> HardAssignment -> Move -> Double -> HardAssignment
sample f a m accept_prop
    | accept_prop < f a m = b -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Accepted: " ++ show (f a m)) b
    | otherwise           = a -- trace ("(" ++ (show $ a!(node m)) ++ "->" ++ (show $ comp m) ++   ")" ++ "Rejected: " ++ show (f a m)) a
                            where b = apply_move a m

getChain :: (HardAssignment -> Move -> Double) -> HardAssignment -> [Move] -> [Double] -> Chain
getChain f start m r = scanl g start l
                       where   g s (m',r') = sample f s m' r'
                               l = zip m r

getElement :: X -> Int -> Int -> Int -> HardAssignment
getElement x seed k i = foldl' g s $ take i $ zip m r
                      where n = length x
                            f = lgaussAccept x
                            s = naivefromXNk n k
                            m = genMoves seed n k
                            r = randomRs (0.0,1.0) (mkStdGen seed) :: [Double]
                            g s' (m', r')= sample f s' m' r'


initGChain :: X -> Int -> Int -> Chain
initGChain x seed k = getChain f s m r
                        where   n = length x
                                f = lgaussAccept x
                                s = naivefromXNk n k
                                m = genMoves seed n k
                                r = randomRs (0.0,1.0) (mkStdGen seed) :: [Double]

select :: X -> HardAssignment -> Int -> X
select x a i =  fst . unzip $ filter (\(_,c)->c==i) $ zip x $ elems a

gaussAccept :: X -> HardAssignment -> Move -> Double
gaussAccept x a m = (l x $ apply_move a m)/(l x a)
                    where l x a = sumAllClusters normalPDFplugin x a

lgaussAccept :: X -> HardAssignment -> Move -> Double
lgaussAccept x a m = --trace (("Prop: " ++ show (l x $ apply_move a m))
                     --       ++ ("\nS: " ++ show (l x a))
                     --       ++ ("\nRatio: " ++ show (exp ((l x $ apply_move a m)-(l x a))))
                     --      )
                           exp ((l x $ apply_move a m)-(l x a))
                     where  l x' a' = sumAllClusters lnormalPrior x' a'

sumAllClusters :: (X -> Double) -> X -> HardAssignment -> Double
sumAllClusters f x a = sum $ map (\i -> f (xs i))  [1..k]
                       where   k = maximum $ elems a
                               xs i = select x a i

lnormalInvWishart :: X -> Double
lnormalInvWishart x = - (n * d * 2 * (log pi))
                      + (mlgamma d (vn/2))
                      - (mlgamma d (v0/2))
                      + (v0/2) * ldalpha0
                      - (vn/2) * ldalphaN
                      + (d/2) * ((log k0)-(log kn))
                    where   n = fromIntegral $ length x :: Double
                            d = fromIntegral $ dim $ head x
                            v0 = 10 -- hyperparameter must be (v0 > d)
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

qtrace x = trace ("value: " ++ show x) x

scatterMatrix :: X -> Matrix Double
scatterMatrix x = foldl (+) (zeros d d) $ map (\ p -> (p-xm)<>(trans (p-xm)) ) matrixX
                    where n = fromIntegral $ length x :: Double
                          d = fromIntegral $ dim $ head x
                          xm = xm2 -- trace (show xm2) xm2
                          xm2 = fromRows $ [ fst $ meanCov $ fromRows x ]
                          matrixX = map (\p -> fromRows [p]) x

lInverseMean :: X -> Double
lInverseMean x = - log (norm xm)
                 where xm = fst $ meanCov $ fromRows x

lInvScatter :: X -> Double
lInvScatter x = -(log $ sumElements s)**3
                where n = fromIntegral $ length x :: Double
                      (_,(absLS,signLS)) = invlndet s
                      lDetS = absLS * signLS
                      s = snd $ meanCov $ fromRows x


lnormalPrior :: X -> Double
lnormalPrior x = -(n/2)*lDetSigma -(1/2)* tr (invSigma <> s)
                where n = fromIntegral $ length x
                      d = dim $ head x
                      tr m = sum $ toList $ takeDiag m
                      s = scatterMatrix x
                      (invSigma,(absLSigma,signLSigma)) = invlndet sigma
                      lDetSigma = absLSigma * signLSigma
                      sigma = ident $ d -- Hyperparameter for the covariance of the cluster

normalPDFplugin :: X -> Double
normalPDFplugin x = 1/((2*pi)^d * determinant)^^n *  (exp $ -1/2 * ( sum (map e x)))
                        where (mu,sigma) = meanCov $ fromRows x
                              d = dim mu
                              n = length x
                              determinant = det sigma
                              e v = ((v-mu)<>(inv sigma))<.>(v-mu)




-- TODO evaluate in log form to mitigate overflow


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


