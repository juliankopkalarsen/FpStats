
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

import           System.CPUTime
import           System.Exit            (exitFailure)
import           System.IO              (readFile)
import           Text.Printf
import           System.Random (mkStdGen)
import           Debug.Trace
import           MCMC
import           Partition
import           Math
import           Distributions (lnormalInvWishartSS, lnormalInvWishart)
import           LikSpecLang

list2Touple (a:b:_) = (a,b)
list2Touple _ = (0,0)

toTouples :: Num a => [[a]] -> [(a,a)]
toTouples = map list2Touple

--to2D = toTouples . toLists
plotClusters :: X -> Partition -> [Graph2D Double Double]
plotClusters x p = map toplot $ range (0,((k p)-1))
                      where toplot i = Data2D [Title "d"] [] (toTouples $ map toList $ select x p i)

qtr x = trace ("value: " ++ show x) x

instance Sampleable Partition X where
    condMove _ r p = move r p
    llikelihood = $(simplify [|\d part -> sum $ map (lnormalInvWishart . (group d)) (p part)|])
    llikDiff  = $((delta . simplify) [|\d part -> sum $ map (lnormalInvWishart . (group d)) (p part)|])

gmmElement :: X -> Int -> Int -> Int -> Partition
gmmElement x seed k num = p
          where p = getElement gen x startP num
                gen = mkStdGen seed
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
    contents <- readFile "../../Data/synthetic.6clust.csv"
    --contents <- readFile "../../Data/synth2c2d.csv"

    let num_Components = 6
        num_Samples = 4000
        stdData = p2NormList contents
        result = gmmElement stdData 1 num_Components
        p a = plot X11 $ plotClusters stdData a

    --Plot Data
    putStrLn "Starting..."
    time $ p (result num_Samples)
    putStrLn "Done."
