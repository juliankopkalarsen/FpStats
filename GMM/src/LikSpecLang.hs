-----------------------------------------------------------------------------
--
-- Module      :  LikSpecLang
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
{-#LANGUAGE GADTs #-}

module LikSpecLang (
    Expr (
        Fun,
        Mixture
        ),
    delta,
    compile
) where

import Data.Function

data Expr i o where
    Fun :: (i->o) -> Expr i o
    Mixture :: (Num o, Eq a) => Expr a o -> Expr [a] o

compile :: Expr i o -> (i -> o)
compile (Fun f) = f
compile (Mixture e) = sum . map (compile e)

delta :: (Num o) => Expr i o -> (i -> i -> o)
delta (Mixture e) = \x' x -> sum $ map (uncurry (delta e)) (changed x' x)
        where changed a b= filter (uncurry (/=)) $ zip a b

delta e = (-) `on` (compile e) -- Works for any case. also Fun f
