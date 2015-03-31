-----------------------------------------------------------------------------
--
-- Module      :  Partition
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

module Partition (
    Partition,
    Move,
    select,
    numComponents,
    naivefromXNk,
    genMoves,
    applyMove
) where

import Data.List
import System.Random

data Move = Move {   node :: Int
                    ,comp :: Int
                    } deriving Show

type Partition = [[Int]]

naivefromXNk :: Int -> Int -> Partition
naivefromXNk n k = transpose $ splitEvery k [0..n-1]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where (first,rest) = splitAt n list

blockFromNk :: Int -> Int -> Partition
blockFromNk n k = fst s : snd s : []
                  where n1 = n `div` k
                        s = splitAt n1 [0..n-1]

applyMove :: Partition -> Move -> Partition
applyMove p m = moveNode c i p
                 where  i = node m
                        c = comp m


moveNode:: Int -> Int -> [[Int]] -> [[Int]]
moveNode 0 i (x:xs)
    | elem i x = x:xs
    | notElem i x = (i:x):(map (delete i) xs)
moveNode n i (x:xs) = (delete i x):(moveNode (n-1) i xs)


applyTo :: (a -> a) -> Int -> [a] -> [a]
applyTo f i l = (take (i-1) l) ++ (f (l!!i)):(drop i l)

genMoves :: Int -> Int -> Int -> [Move]
genMoves seed n k = zipWith (\i c -> Move {node=i, comp=c})
                            (randomRs (0,n-1) (mkStdGen seed))
                            (randomRs (0,k-1) (mkStdGen seed))


select :: [a] -> Partition -> Int -> [a]
select x p i = map (x!!) (p!!i)

numComponents :: Partition -> Int
numComponents = length
