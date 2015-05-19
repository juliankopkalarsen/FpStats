
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Main
Description : Main module running the GMM
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

-}
module Main (
    main
) where

import           Control.Monad          (forM, unless)
import           Data.List              (transpose, (\\))
import           FileParsing
import           GHC.Arr (range)
import           Graphics.EasyPlot
import           Numeric.LinearAlgebra
-- import           Numeric.Statistics.PCA
import           System.CPUTime
import           System.Exit            (exitFailure)
import           System.IO              (readFile)
import           Text.Printf
import           System.Random (mkStdGen)
import Debug.Trace


import           MCMC
import           Partition
import           Math
import           Distributions (lNormW, dlNormW,lnormalInvWishartSS)

list2Touple (a:b:_) = (a,b)
list2Touple _ = (0,0)

toTouples :: Num a => [[a]] -> [(a,a)]
toTouples = map list2Touple

--to2D = toTouples . toLists
plotClusters :: X -> Partition -> [Graph2D Double Double]
plotClusters x p = map toplot $ range (0,((k p)-1))
                      where toplot i = Data2D [Title "d"] [] (toTouples $ map toList $ select x p i)

qtr x = trace ("value: " ++ show x) x

data State = State (Partition,[Sstat]) deriving Show

instance Sampleable State X where
    condMove x g (State (p,s)) = State (newPar, test)
                  where newPar = move g p
                        update = ss' x p s newPar
                        recalc = ss x newPar
                        test | update == recalc = update
                             | otherwise = error ("update not working, \n recalc: " ++ show recalc ++ "\n update: " ++ show update)

    llikelihood _ (State (_,s)) = sum $ map (\(i,m,v) -> lnormalInvWishartSS i m v) s
    llikDiff _ (State (_,s')) (State (_,s))
                = f s' - f s
                where f s = sum $ map (\(i,m,v) -> lnormalInvWishartSS i m v) s

ss :: X -> Partition -> [Sstat]
ss x part = zip3 csizes scatters means
            where csizes = componentSizes part
                  scatters = map scatterMatrix xs
                  means = map meanv xs
                  xs = groups x part

ss' :: X -> Partition -> [Sstat] -> Partition -> [Sstat]
ss' x par s npar = map go $ zip3 (p par) s (p npar)
        where go (c1, s, c2)
               | c1 == c2 = s
               | otherwise = updateSstat x c1 s c2

ssComponent :: X -> [Int] -> Sstat
ssComponent x []= (0, konst 0 (d,d), konst 0 d)
                where  d = dim $ head x
ssComponent x c = (length c, scatterMatrix xs, meanv xs)
                where  xs = group x c

updateSstat :: X -> [Int] -> Sstat -> [Int] -> Sstat
updateSstat x c1 s c2 = (s <-> removed) <+> added
                where removed =  (ssComponent x (c1\\c2))
                      added =   (ssComponent x (c2\\c1))

gmmElement :: X -> Int -> Int -> Int -> Partition
gmmElement x seed k num = p
          where State (p,_) = getElement gen x start num
                gen = mkStdGen seed
                start = State (startP, ss x startP)
                startP = naiveFromNk n k
                n = length x

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

-- Main
main = do
    --contents <- readFile "../../Data/AccidentData.csv"
    --contents <- readFile "../../Data/2Clusters.csv"
    --contents <- readFile "../../Data/synthetic.6clust.csv"
    contents <- readFile "../../Data/synth2c2d.csv"

    let num_Components = 2
        num_Samples = 4000
        stdData = p2NormList contents
        result = gmmElement stdData 1 num_Components
        p a = plot X11 $ plotClusters stdData a

    --Plot Data
    putStrLn "Starting..."
    time $ p (result num_Samples)
    putStrLn "Done."
