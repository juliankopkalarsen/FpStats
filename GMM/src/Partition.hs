{-|
Module      : Partition
Description : Definition of the Partition type and associated functions.
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

Definition of the Partition type and associated functions.
-}
module Partition (
    Partition(p, n, k),
    Component,
    Move,
    select,
    naiveFromNk,
    blockFromNk,
    genMoves,
    applyMove,
    group,
    groups
) where

import Data.List (transpose,
                  delete,
                  (\\))

import MCMC (Moveable, move)

import System.Random

-- | A 'Move' repressenting the move of an element into a component.
data Move = Move {node :: Int -- ^ The 'node' to be moved.
                 ,comp :: Int -- ^ The destination component 'comp'.
                  } deriving Show

data Partition = Partition {p::[Component]
                        ,n::Int, k::Int} deriving Show



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

-- |Apply a 'Move' to a 'Partition' to create a new 'Partition'
applyMove :: Partition -> Move -> Partition
applyMove part m = Partition {p = moveNode c i (p part), n=n part, k=k part}
                 where  i = node m
                        c = comp m

-- |move a node from one 'Component' to the other
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

instance Moveable Partition where
    move g part = Partition {p=moveNode randC randN (p part), n=n part, k=k part }
            where (g1,g2) = split g
                  randC = randComp part g1
                  randN = randNode part g2

-- | Extract a speccific component from a dataset given a 'Partition' and an index.
select :: [a] -> Partition -> Int -> [a]
select x part i = group x (p part!!i)

-- | Extract one component from a dataset.
group :: [a] -> Component -> [a]
group x = map (x!!)

-- | Extract all of the components from a dataset.
groups :: [a] -> Partition -> [[a]]
groups x part = map (group x) (p part)
