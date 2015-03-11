-----------------------------------------------------------------------------
--
-- Module      :  FIleParsing
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

module FileParsing (
    parseToArray,
    parseToMatrix,
    parseToDouble,
    normalizeColumns,
    p2NormArray,
    p2NormList,
    matrix2Array
) where

import Text.Parsec
import Data.Array
import Data.Packed.Vector
import Numeric.LinearAlgebra
import Numeric.Statistics

-- Csv parser
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseToStrings :: String -> [[String]]
parseToStrings input = case parse csvFile "(unknown)" input of
                        Left e -> []
                        Right c -> c

parseToDouble :: String -> [[Double]]
parseToDouble input = map (\l -> map read l) . tail $ parseToStrings input

parseToMatrix :: String -> Matrix Double
parseToMatrix = fromLists . parseToDouble

matrix2Array :: Matrix Double -> Array Int (Vector Double)
matrix2Array m = listArray (1,n) r
                 where   r = toRows m
                         n = length r

parseToArray :: String -> Array Int (Vector Double)
parseToArray = matrix2Array . parseToMatrix

normalizeColumns :: Matrix Double -> Matrix Double
normalizeColumns m = fromColumns $ map studentize (toColumns m)

p2NormArray :: String -> Array Int (Vector Double)
p2NormArray = matrix2Array . normalizeColumns . parseToMatrix

p2NormList :: String -> [Vector Double]
p2NormList = toRows . normalizeColumns . parseToMatrix

















