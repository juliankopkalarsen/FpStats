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
main = do
    --contents <- readFile "../../Data/AccidentData.csv"
    --contents <- readFile "../../Data/2Clusters.csv"
    --contents <- readFile "../../Data/synthetic.6clust.csv"
    contents <- readFile "../../Data/synth2c2d.csv"

    let num_Components = 2
        num_Samples = 20000
        stdData = p2NormList contents
        result = getElement stdData 1 num_Components
        p a = plot X11 $ plotClusters a stdData

    --Plot Data
    putStrLn "Starting..."
    time $ p (result num_Samples)
    putStrLn "Done."

    --fib n = (xs!!(n-1)) + (xs!!(n-2))
    --        where xs = 0:1:map fib [2..]



