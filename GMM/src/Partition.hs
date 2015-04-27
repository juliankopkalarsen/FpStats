{-|
Module      : Partition
Description : Definition of the Partition type and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Definition of the Partition type and associated functions.
-}
module Partition (
    Partition,
    Component,
    Move,
    select,
    numComponents,
    naivefromNk,
    genMoves,
    applyMove,
    group,
    groups
) where

import Data.List (transpose,
                  delete,
                  (\\))

import System.Random

-- | A 'Move' repressenting the move of an element into a component.
data Move = Move {node :: Int -- ^ The 'node' to be moved.
                 ,comp :: Int -- ^ The destination component 'comp'.
                  } deriving Show

type Partition = [Component]

type Component = [Int]

-- | Constructs a 'Partition' with elements assigned to components in cyclic order.
naivefromNk :: Int -- ^ The n number of elements
            -> Int -- ^ The k number of components
            -> Partition
naivefromNk n k = transpose $ splitEvery k [0..n-1]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where (first,rest) = splitAt n list

-- | Constructs a 'Partition' with elements split in k blocks one after the other.
blockFromNk :: Int -- ^ The n number of elements
            -> Int -- ^ The k number of components
            -> Partition
blockFromNk n k = [fst s, snd s]
                  where n1 = n `div` k
                        s = splitAt n1 [0..n-1]

-- |Apply a 'Move' to a 'Partition' to create a new 'Partition'
applyMove :: Partition -> Move -> Partition
applyMove p m = moveNode c i p
                 where  i = node m
                        c = comp m

moveNode:: Int -> Int -> [[Int]] -> [[Int]]
moveNode 0 i (x:xs)
    | i `elem` x = x:xs
    | i `notElem` x = (i:x):(map (delete i) xs)
moveNode n i (x:xs) = (delete i x):(moveNode (n-1) i xs)

applyTo :: (a -> a) -> Int -> [a] -> [a]
applyTo f i l = (take (i-1) l) ++ (f (l!!i)):(drop i l)

genMoves :: Int -> Int -> Int -> [Move]
genMoves seed n k = zipWith (\i c -> Move {node=i, comp=c})
                            (randomRs (0,n-1) (mkStdGen seed))
                            (randomRs (0,k-1) (mkStdGen seed))

-- | Extract a speccific component from a dataset given a 'Partition' and an index.
select :: [a] -> Partition -> Int -> [a]
select x p i = group x (p!!i)

-- | The number of components in a partition.
numComponents :: Partition -> Int
numComponents = length

-- | Extract one component from a dataset.
group :: [a] -> Component -> [a]
group x = map (x!!)

-- | Extract all of the components from a dataset.
groups :: [a] -> Partition -> [[a]]
groups x = map (group x)
