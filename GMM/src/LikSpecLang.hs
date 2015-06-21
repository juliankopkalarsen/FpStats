{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
    delta,
    declareSampelable,
    splitCacheableQ,
    multiFunctionPassThrough
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)
import Control.Monad (liftM)
import Data.List
import Math
import MCMC

import Debug.Trace

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
    simplify (AppE (AppE (VarE fname) e1) e2) | fname == '(.) = simplify (LamE [xP] (AppE e1 (AppE e2 xE)))
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
    delta (LamE [dP@(VarP d),pP@(VarP p)] (AppE sumE (AppE (AppE mapE  f) l))) =
                    LamE [dP, (TupP [pP, p'P] ) ] (AppE sumE (AppE (AppE mapE (delta f)) changed))
            where p' = alternativeName p
                  p'P = VarP p'
                  pE = VarE p
                  p'E = VarE p'
                  z = AppE (AppE zipE l) (replaceName (p,p') l)
                  changed = AppE (AppE filterE (AppE uncurryE neqE)) z

    delta e@(LamE [VarP a, bP] _) = LamE [VarP a, (TupP [VarP b, VarP b'] ) ] (AppE (AppE (VarE (mkName "-")) l') l)
            where b = mkName "b"
                  b' = mkName "b'"
                  l = AppE (AppE e (VarE a)) (VarE b)
                  l' = AppE (AppE e (VarE a)) (VarE b')

    delta (LamE (WildP:ps) e) = case delta (LamE ps e) of
                                LamE ps e -> LamE (WildP:ps) e

    delta e = LamE [(TupP [VarP b, VarP b'] ) ] (AppE (AppE minusE l') l)
            where b = mkName "b"
                  b' = mkName "b'"
                  l = AppE  e (VarE b)
                  l' = AppE e (VarE b')

alternativeName :: Name -> Name
alternativeName name = mkName ((showName name) ++ "'")

filterE = VarE 'filter
uncurryE = VarE 'uncurry
sumE = VarE 'sum
mapE = VarE 'map
dollarE = VarE '($)
zipE = VarE 'zip
neqE = VarE '(/=)
minusE = VarE '(-)
funCompE = VarE '(.)

replaceNameWithAlternate :: Exp -> Exp
replaceNameWithAlternate (VarE n) = VarE (alternativeName n)
replaceNameWithAlternate (ConE n) = ConE (alternativeName n)
replaceNameWithAlternate (AppE e1 e2) = AppE (replaceNameWithAlternate e1) (replaceNameWithAlternate e2)
replaceNameWithAlternate e = e

replaceName :: (Name,Name) -> Exp -> Exp
replaceName (n,n') (VarE n1) | n1==n = VarE n'
replaceName (n,n') (ConE n1) | n1==n = ConE n'
replaceName names (AppE e1 e2) = AppE (replaceName names e1) (replaceName names e2)
replaceName names (LamE ps exp) = LamE (map (replaceNameP names) ps) (replaceName names exp)
replaceName (n,n') e = e

replaceNameP :: (Name,Name) -> Pat -> Pat
replaceNameP (n,n') (VarP n1) | n1==n = VarP n'
replaceNameP (n,n') p = p

replaceExp :: (Exp,Exp) -> Exp -> Exp
replaceExp (e, e') (AppE e1 e2) | e1 == e = replaceExp (e, e') (AppE e' e2)
                                | e2 == e = AppE e1 e'
                                | otherwise = AppE (replaceExp (e, e') e1) (replaceExp (e, e') e2)
replaceExp es (LamE ps exp) = LamE ps (replaceExp es exp)
replaceExp _ e = e


-- | Instance declaration

declareSampelable :: Name -> Name -> Q Exp -> Q Exp -> Q Exp -> Q [Dec]
declareSampelable aName dName start prop llik = [d| instance Sampleable $(conT aName) $(conT dName) where
                                                        startValue = $(start)
                                                        proposeMove = $(simplify prop)
                                                        llikelihood = $(llik)
                                                        llikDiff = $(delta $ simplify llik)
                                                    |]

multiFunctionPassThrough :: Q [Dec] -> Q [Dec]
multiFunctionPassThrough qdec = qdec >>= f
    where f [(ValD _  (NormalB (LamE [VarP xN, VarP rN, VarP aN] (TupE [proposal, moveRatio]))) []), (ValD _  (NormalB erE) []), (ValD _  (NormalB stE) []), (ValD _  (NormalB endE) [])] = (splitCacheableQ $ simplify $ return erE) >>= g
            where g (Just a, Just (LamE [VarP dN, VarP pN] b)) = do
                                gpE <- return (LamE [VarP xN, VarP rN, TupP [VarP aN, VarP cN]] (TupE [(TupE [proposal, (replaceExp (VarE pN ,proposal) (replaceName (dN, xN) b))]), moveRatio]))
                                return [defineLfun "genPropX" gpE,
                                        defineLfun "evalRatioX" (LamE [WildP] (delta $ cached a)),
                                        defineLfun "startX" (cStart dN pN stE b),
                                        defineLfun "end" (LamE [TupP [xP, WildP]] (AppE endE xE))]
                  g _ = return [defineLfun "genPropX" (LamE [VarP xN, VarP rN, VarP aN] (TupE [proposal, moveRatio])),
                                defineLfun "evalRatioX"  (delta $ simplify erE),
                                defineLfun "startX" stE,
                                defineLfun "end" endE]

cN = mkName "cache"

--(Just (LamE [VarP x] (AppE (VarE Data.List.sum) (AppE (AppE (VarE GHC.Base.map) (VarE Distributions.lnormalInvWishartSS)) (VarE x)))),
-- Just (LamE [VarP d_2,VarP part_3] (AppE (AppE (VarE GHC.Base.map) (LamE [VarP x] (AppE (LamE [VarP x] (AppE (VarE Math.fromData) (AppE (AppE (VarE Partition.group) (VarE d_2)) (VarE x)))) (VarE x)))) (AppE (VarE Partition.p) (VarE part_3)))))

defineLfun :: String -> Exp -> Dec
defineLfun s e = FunD (mkName s) [Clause [] (NormalB e) []]

cached :: Exp -> Exp
cached (LamE [p] e) = LamE [(TupP [WildP, p])] e

cStart :: Name -> Name -> Exp -> Exp -> Exp
cStart dname pname (LamE [d] e) (LamE [_, _] b) = (LamE [d] (TupE [e, (AppE b e)]))
cStart dname pname (LamE [d@(VarP dN)] e) b = (LamE [d] (TupE [e, (replaceExp (VarE pname ,e) (replaceName (dname, dN) b))]))

--declare :: Name -> Name -> Q Exp -> Q Exp -> Q Exp -> Q [Dec]
--declare aName dName start prop llik = case (splitCacheable f) of
--                                       (Nothing, Nothing)-> declareSampelable aName dName start prop llik
--                                       (Just a, Just b)  -> declareSampelable aName dName start prop llik
--                                       (Nothing, Just b) -> declareSampelable aName dName start prop llik
--                                       (Just a, Nothing) -> declareSampelable aName dName start prop llik
--

--(Just (AppE (VarE Data.List.sum) (AppE (VarE GHC.Base.map) (VarE Distributions.lnormalInvWishartSS))),
-- Just (LamE [VarP d_2,VarP part_3] (AppE (AppE (VarE GHC.Base.map) (LamE [VarP x_123] (AppE (LamE [VarP x_123] (AppE (VarE Math.fromData) (AppE (AppE (VarE Partition.group) (VarE d_2)) (VarE x_123)))) (VarE x_123)))) (AppE (VarE Partition.p) (VarE part_3)))))


splitCacheableQ :: Q Exp -> Q (Maybe Exp, Maybe Exp)
splitCacheableQ = liftM splitCacheable

splitCacheable :: Exp -> (Maybe Exp, Maybe Exp)
splitCacheable l@(AppE (AppE (VarE name) f) x) | name == 'map = case (splitCacheable f) of
                                                               (Nothing, Nothing)-> (Nothing, Nothing)
                                                               (Just a, Just b)  -> (Just (AppE mapE a), Just (AppE (AppE mapE b) x))
                                                               (Nothing, Just b) -> (Nothing , Just l)
                                                               (Just a, Nothing) -> case (splitCacheable x) of
                                                                                    (Nothing, Nothing) -> (Nothing, Nothing)
                                                                                    (Just a', Just b') -> (Just (AppE (AppE mapE f) a'), Just b')
                                                                                    (Nothing, Just b') -> (Just (AppE mapE f), Just b')
                                                                                    (Just a', Nothing) -> (Just l, Nothing)
splitCacheable l@(AppE f x) = case (splitCacheable x) of
                               (Nothing, Nothing)-> (Nothing, Nothing)
                               (Just a, Just b)  -> (Just (LamE [xP] (AppE f (AppE a xE))), Just b)
                               (Nothing, Just b) -> (Just f , Just b)
                               (Just a, Nothing) -> case (splitCacheable f) of
                                                    (Nothing, Nothing)-> (Nothing, Nothing)
                                                    (Just a', Just b')  -> (Just a', Just (AppE b' x))
                                                    (Nothing, Just b') -> (Nothing , Just l)
                                                    (Just a', Nothing) -> (Just l, Nothing)

splitCacheable l@(LamE ps e) = case (splitCacheable e) of
                               (Nothing, Nothing)->  (Nothing, Nothing)
                               (Just a, Just b) -> (Just a, Just (LamE ps b))
                               (Nothing, Just b) -> (Nothing , Just l)
                               (Just a, Nothing) -> (Just l, Nothing)

splitCacheable (VarE fname) | fname == 'fromData = (Nothing, Just fromDataE)
splitCacheable e = (Just e, Nothing)

fromDataE = VarE 'fromData
xE = VarE (mkName "x")
xP = VarP (mkName "x")









