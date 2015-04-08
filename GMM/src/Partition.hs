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
    Component,
    Move,
    select,
    numComponents,
    naivefromXNk,
    genMoves,
    applyMove,
    group,
    groups
) where

import Data.List (transpose,
                  delete,
                  (\\))

import System.Random

data Move = Move {   node :: Int
                    ,comp :: Int
                    } deriving Show

type Partition = [Component]
                -- |Delta Partition Partition
                -- deriving Eq
type Component = [Int]


naivefromXNk :: Int -> Int -> Partition
naivefromXNk n k = transpose $ splitEvery k [0..n-1]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where (first,rest) = splitAt n list

blockFromNk :: Int -> Int -> Partition
blockFromNk n k = [fst s, snd s]
                  where n1 = n `div` k
                        s = splitAt n1 [0..n-1]

applyMove :: Partition -> Move -> Partition
applyMove p m = moveNode c i p
                 where  i = node m
                        c = comp m

moveNode:: Int -> Int -> [[Int]] -> [[Int]]
moveNode 0 i (x:xs)
    | i `elem` x = x:xs
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

group :: [a] -> Component -> [a]
group x = map (x!!)

groups :: [a] -> Partition -> [[a]]
groups x = map (group x)
