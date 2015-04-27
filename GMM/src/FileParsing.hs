{-|
Module      : FileParsing
Description : Definition of Parsers for csv-files
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Various Functions that provide parsing of CSV files.
-}
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

-- | Parse a File to a list of lists of elements.
parseToStrings :: String -> [[String]]
parseToStrings input = case parse csvFile "(unknown)" input of
                        Left e -> []
                        Right c -> c

-- | Read a csv string to List of Lists of 'Double'
parseToDouble :: String -> [[Double]]
parseToDouble input = map (map read) . tail $ parseToStrings input

-- | Read a csv string into a 'Matrix' of 'Double'
parseToMatrix :: String -> Matrix Double
parseToMatrix = fromLists . parseToDouble

-- | Convert a 'Matrix Double' into an 'Array' of 'Vector Double'
matrix2Array :: Matrix Double -> Array Int (Vector Double)
matrix2Array m = listArray (1,n) r
                 where   r = toRows m
                         n = length r

-- | Parse a csv 'String' into an 'Array' of 'Vector Double'
parseToArray :: String -> Array Int (Vector Double)
parseToArray = matrix2Array . parseToMatrix

-- | Normalize (alt. Studetize) the Collumns in a 'Matrix'
normalizeColumns :: Matrix Double -> Matrix Double
normalizeColumns m = fromColumns $ map studentize (toColumns m)

-- | Parse a csv 'String' into a Normalized 'Array' of 'Vector Double'
p2NormArray :: String -> Array Int (Vector Double)
p2NormArray = matrix2Array . normalizeColumns . parseToMatrix

-- | Parse a csv 'String' into a Normalized 'List' of 'Vector Double'
p2NormList :: String -> [Vector Double]
p2NormList = toRows . normalizeColumns . parseToMatrix

















