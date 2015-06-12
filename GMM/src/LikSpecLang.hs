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
    simplify (LamE ps e) = LamE ps (simplify e)
    simplify (AppE (AppE (VarE fname) e1) e2) | fname == '($) = simplify (AppE e1 e2)
    simplify (InfixE (Just e1) f (Just e2)) = simplify (AppE (AppE f e1) e2)
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
    delta (LamE [dP@(VarP d),pP@(VarP p)] (AppE (VarE sumName) (AppE (AppE (VarE mapName)  f) l)))
            | sumName=='sum && mapName == 'map = LamE [dP, (TupP [pP, p'P] ) ] (AppE (VarE sumName) (AppE (AppE (VarE mapName) (delta f)) changed))
            where p' = mkName "p'"
                  p'P = VarP p'
                  pE = VarE p
                  p'E = VarE p'
                  z = AppE (AppE (VarE (mkName "zip")) l) (replaceName (p,p') l)
                  changed = AppE (AppE filterE (AppE uncurryE (VarE (mkName "/=")))) z

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

filterE = VarE (mkName "filter")
uncurryE = VarE (mkName "uncurry")



replaceName :: (Name,Name) -> Exp -> Exp
replaceName (n,n') (VarE n1) | n1==n = VarE n'
replaceName (n,n') (ConE n1) | n1==n = ConE n'
replaceName names (AppE e1 e2) = AppE (replaceName names e1) (replaceName names e2)
replaceName (n,n') e = e







