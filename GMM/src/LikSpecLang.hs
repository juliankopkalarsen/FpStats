{-|
Module      : LikSpecLang
Description : Definition of the Likelihood Specification Language
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

In this module the Likelihood Specification Language is defined.
-}

{-#LANGUAGE GADTs #-}


module LikSpecLang (
    simplify,
    reduce,
    delta
) where

import Language.Haskell.TH
import Control.Monad (liftM)

class Simplify a where
    simplify :: a -> a

instance Simplify a => Simplify (Q a) where
    simplify = liftM simplify

instance Simplify a => Simplify [a] where
    simplify = map simplify

instance Simplify Exp where
    simplify (AppE e1 e2) = AppE (simplify e1) (simplify e2)
    simplify e = e

reduce :: Exp -> Exp
reduce (LamE [VarP a, VarP b] e) = LamE [VarP a, VarP b] e
reduce exp = exp

-- | 'delta' compiles an expression to a haskell function that computes a difference.
delta :: Exp -> Exp
delta e@(LamE [VarP a, VarP b] _) = [|\a b b' -> ($e a b') - ($e a b) |]
--delta e@(LamE [VarP a, VarP b] _) = LamE [VarP a, VarP b, VarP b'] (AppE (AppE (VarE (mkName "-")) l') l)
 --   where b' = mkName "b'"
 --         l = AppE (AppE e (VarE a)) (VarE b)
  --        l' = AppE (AppE e (VarE a)) (VarE b')

