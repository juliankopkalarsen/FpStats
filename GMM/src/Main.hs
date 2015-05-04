
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
import           Data.List              (transpose)
import           FileParsing
import           GHC.Arr
import           Graphics.EasyPlot
import           Numeric.LinearAlgebra
-- import           Numeric.Statistics.PCA
import           System.CPUTime
import           System.Exit            (exitFailure)
import           System.IO              (readFile)
import           Text.Printf

import           GMM
import           Partition

list2Touple (a:b:_) = (a,b)
list2Touple _ = (0,0)

toTouples :: Num a => [[a]] -> [(a,a)]
toTouples = map list2Touple

--to2D = toTouples . toLists
plotClusters :: X -> Partition -> [Graph2D Double Double]
plotClusters x p = map toplot $ range (0,(k-1))
                      where toplot i = Data2D [Title "d"] [] (toTouples $ map toList $ select x p i)
                            k = numComponents p


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
        result = getElement stdData 1 num_Components
        p a = plot X11 $ plotClusters stdData a

    --Plot Data
    putStrLn "Starting..."
    time $ p (result num_Samples)
    putStrLn "Done."
