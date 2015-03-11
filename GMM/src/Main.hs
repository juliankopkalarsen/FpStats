{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Control.Monad (unless, forM)
import System.IO (readFile)
import System.CPUTime
import Text.Printf
import Data.List (transpose)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Graphics.EasyPlot
import GHC.Arr
import FileParsing
import GMM
import Numeric.Statistics.PCA
import Numeric.LinearAlgebra

list2Touple (a:b:_) = (a,b)
list2Touple _ = (0,0)

toTouples :: Num a => [[a]] -> [(a,a)]
toTouples = map list2Touple

--to2D = toTouples . toLists

plotClusters assign x = map toplot $ range (1,maximum $ elems assign)
                      where toplot i = Data2D [Title "d"] [] (toTouples $ map toList $ select i)
                            select i =  fst . unzip $ filter (\(_,c)->c==i) $ zip x $ elems assign

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

-- Main
exeMain = do
    --contents <- readFile "../../Data/AccidentData.csv"
    --contents <- readFile "../../Data/2Clusters.csv"
    --contents <- readFile "../../Data/synthetic.6clust.csv"
    contents <- readFile "../../Data/synth2c2d.csv"

    let num_Components = 2
        num_Samples = 20000
        stdData = p2NormList contents
        result = getElement stdData 1 num_Components num_Samples

    print $ scatterMatrix stdData
    --Plot Data

    putStrLn "Starting..."
    time $ plot X11 $ plotClusters result stdData
    putStrLn "Done."

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

