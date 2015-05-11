{-|
Module      : LikSpecLang
Description : Definition of the Likelihood Specification Language
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

In this module the Likelihood Specification Language is defined.
-}

{-#LANGUAGE GADTs #-}

module LikSpecLang (
    Expr (
        Fun,
        Mixture,
        Pipe
        ),
    mix,
    (%),
    delta,
    compile
) where

import Data.Function

-- |The expression 'Expr' type defines the syntax of the language
data Expr i o where
    Fun :: (i->o) -> Expr i o -- The 'Fun' expression captures an arbitrary function.
    Mixture :: (Num o, Eq a) => Expr a o -> Expr [a] o -- The 'Mixture' expression describes a mixture of an expression over a list of inputs.
    Pipe :: Expr a o -> Expr i a -> Expr i o

(%) :: Expr a o -> Expr i a -> Expr i o
b % a = Pipe b a

mix :: (Num o, Eq i) => (i->o) -> Expr [i] o
mix f = Mixture (Fun f)

-- | 'compile' turns an 'Expr' type value into its corresponding haskell function.
compile :: Expr i o -> (i -> o)
compile (Fun f) = f
compile (Mixture e) = sum . map (compile e)
compile (Pipe a b) = (compile a) . (compile b)

-- | 'delta' compiles an expression to a haskell function that computes a difference.
delta :: (Num o) => Expr i o -> (i -> i -> o)
delta (Pipe a b) = \x' x -> f' (g x') (g x)
        where f' = delta a
              g = compile b

delta (Mixture e) = \x' x -> sum $ map (uncurry (delta e)) (changed x' x)
        where changed a b = filter (uncurry (/=)) $ zip a b

delta e = \x' x -> (f x') - (f x)
        where f = compile e
