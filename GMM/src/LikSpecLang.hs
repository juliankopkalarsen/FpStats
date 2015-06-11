{-# LANGUAGE TemplateHaskell, QuasiQuotes#-}
{-|
Module      : LikSpecLang
Description : Definition of the Likelihood Specification Language
Copyright   : (c) Julian Kopka Larsen, 2015
Stability   : experimental

In this module the Likelihood Specification Language is defined.
-}

--{-#LANGUAGE GADTs #-}


module LikSpecLang (
    simplify,
    delta
) where

import Language.Haskell.TH
import Control.Monad (liftM)
import Data.List


-- | Simplify class definitions
class Simplify a where
    simplify :: a -> a

instance Simplify a => Simplify (Q a) where
    simplify = liftM simplify

instance Simplify a => Simplify [a] where
    simplify = map simplify

instance Simplify Exp where
    simplify (AppE e1 e2) = AppE (simplify e1) (simplify e2)
    simplify e = e

-- | Delta definitions. 'delta' converts an expression to a haskell function that computes a difference.
class Delta a where
    delta :: a -> a

instance Delta a => Delta (Q a) where
    delta = liftM delta

instance Delta a => Delta [a] where
    delta = map delta

instance Delta Exp where
    delta (LamE [VarP d,VarP p] (InfixE (Just (InfixE (Just (VarE sum)) (VarE (.)) (Just (AppE (VarE map) (f))))) (VarE ($)) (Just (AppE (AppE (select) (dE)) (pE))))) = LamE [VarP d, (TupP [VarP p, VarP p'] ) ] (AppE (AppE (AppE (VarE (.)) (VarE sum)) (AppE (VarE map) (delta f))) changed)
            where p' = mkName "p'"
                  pE = VarE p
                  p'E = VarE p'
                  selectOnD = (AppE (select) (VarE d))
                  z = AppE (AppE (VarE (mkName "zip")) (AppE selectOnD pE)) (AppE selectOnD p'E)
                  changed = AppE (AppE (VarE (mkName "filter")) (AppE (VarE (mkName "uncurry")) (VarE (mkName "/=")))) z

    delta e@(LamE [VarP a, VarP b] _) = LamE [VarP a, (TupP [VarP b, VarP b'] ) ] (AppE (AppE (VarE (mkName "-")) l') l)
            where a = mkName "a"
                  b = mkName "b"
                  b' = mkName "b'"
                  l = AppE (AppE e (VarE a)) (VarE b)
                  l' = AppE (AppE e (VarE a)) (VarE b')
    delta e= LamE [(TupP [VarP b, VarP b'] ) ] (AppE (AppE (VarE (mkName "-")) l') l)
            where b = mkName "b"
                  b' = mkName "b'"
                  l = AppE  e (VarE b)
                  l' = AppE e (VarE b')




-- qdelta :: Q Exp -> Q Exp
-- qdelta e = [|\d (p, p') -> ($e d p')-($e d p) |]
-- pattern matching in Q with quasiquotes is not available

--LamE [VarP d,VarP p] (InfixE (Just (InfixE (Just (VarE Data.List.sum)) (VarE .) (Just (AppE (VarE map) (f))))) (VarE $) (Just (AppE (AppE (ss) (VarE d)) (VarE p))))


--LamE [VarP k_15,VarP f_16] (AppE (AppE (AppE (VarE GHC.Base..) (VarE Data.List.sum)) (AppE (VarE GHC.Base.map) fE)) lE)

