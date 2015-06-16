{-|
Module      : Partition
Description : Definition of the Partition type and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Definition of the Partition type and associated functions.
-}
module Partition (
    Partition(Partition, p, n, k),
    componentSizes,
    Component,
    select,
    naiveFromNk,
    blockFromNk,
    group,
    groups,
    allPartitions,
    countPartitions,
    partToSetPart,
    setPartToPart
) where

import Data.List (transpose,
                  delete,
                  (\\),
                  sort,
                  sortBy)

import Data.Function (on)

import MCMC (Moveable, move)

import System.Random
import Debug.Trace
import qualified Math.Combinat.Partitions.Set as SP

qtr x = trace ("value: " ++ show x) x

data Partition = Partition {p::[Component]
                        ,n::Int, k::Int} deriving (Ord, Eq, Show)

componentSizes :: Partition -> [Int]
componentSizes = (map length) . p

randComp :: (RandomGen r) => Partition -> r -> Int
randComp part g = fst $ randomR (0,(k part)-1) g

randNode :: (RandomGen r) => Partition -> r -> Int
randNode part g = fst $ randomR (0,(n part)-1) g

type Component = [Int]

-- | Constructs a 'Partition' with elements assigned to components in cyclic order.
naiveFromNk :: Int -- ^ The n number of elements
            -> Int -- ^ The k number of components
            -> Partition
naiveFromNk n k = Partition {p=transpose $ splitEvery k [0..n-1], n=n, k=k}

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where (first,rest) = splitAt n list

-- | Constructs a 'Partition' with elements split in k blocks one after the other.
blockFromNk :: Int -- ^ The n number of elements
            -> Int -- ^ The k number of components
            -> Partition
blockFromNk n k = Partition {p=[fst s, snd s], n=n, k=k}
                  where n1 = n `div` k
                        s = splitAt n1 [0..n-1]

-- |move a node from one 'Component' to the other
moveNode:: Int -> Int -> [[Int]] -> [[Int]]
moveNode 0 i (x:xs)
        | i `elem` x = x:xs
        | i `notElem` x = (i:x):(map (delete i) xs)
moveNode n i (x:xs) = (delete i x):(moveNode (n-1) i xs)
moveNode _ _ [] = trace ("partition is an empty list" ) []

instance Moveable Partition where
    move g part = Partition {p=pu, n=n part, k=k part }
            where (g1,g2) = split g
                  randC = randComp part g1
                  randN = randNode part g2
                  p' = moveNode randC randN (p part)
                  pu = p $ setPartToPart (k part) $ partToSetPart (Partition p' (n part) (k part))

-- | Extract a speccific component from a dataset given a 'Partition' and an index.
select :: Show a => [a] -> Partition -> Int -> [a]
select x part i = group x (p part!!i)

-- | Extract one component from a dataset.
group :: Show a => [a] -> Component -> [a]
group d c = go  d   (sort c) 0 []
      where go  _       []   _ m = m
            go (x:xs) (k:ks) i m
                | k==i = go xs ks (i+1) (x:m)
                | otherwise = go xs (k:ks) (i+1) m
            go [] (k:ks) i m = trace ("data overflow: looking for " ++ show (k:ks) ++ "in an empty list" ) m


-- | Extract all of the components from a dataset.
groups :: Show a => [a] -> Partition -> [[a]]
groups x part = map (group x) (p part)

countPartitions n _ = SP.countSetPartitions n

allPartitions :: Int -> Int -> [Partition]
allPartitions n k = map (\p' -> Partition {p=p', n=n, k=k}) $ allPartitionsUnwrapped n k

allPartitionsUnwrapped :: Int -> Int -> [[[Int]]]
allPartitionsUnwrapped _ 0 = []
allPartitionsUnwrapped n k = ( map (map (map (\x -> x-1))) $ map SP.fromSetPartition $ SP.setPartitionsWithKParts k n) ++ m
                    where m = ( map (\p -> p++[[]]) $ allPartitionsUnwrapped n (k-1))

partToSetPart :: Partition -> SP.SetPartition
partToSetPart (Partition p n k) = SP.toSetPartition p''
        where p' = (map (map (\x -> x+1))) p
              p'' = stripEmpty p'

setPartToPart :: Int -> SP.SetPartition -> Partition
setPartToPart k (SP.SetPartition p) = Partition p'' n k
            where n = sum $ map length p
                  p' = (map (map (\x -> x-1))) p
                  p'' = padEmpty k p'



sp2p k sp = padEmpty k $ toZeroIdx $ SP.fromSetPartition $ SP.toSetPartition $ stripEmpty sp
          where toZeroIdx = (map (map (\x -> x-1)))

padEmpty :: Int -> [[Int]] -> [[Int]]
padEmpty k s | length s == k = s
             | length s < k = s ++ (replicate (k - (length s)) [])

stripEmpty :: Eq a => [[a]] -> [[a]]
stripEmpty = filter (/=[])





