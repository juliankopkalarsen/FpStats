{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
--
-- Module      :  Math.SummaryStats
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

module Math.SummaryStats (
    SStat(s, s')
) where

class SStat i o where
    s :: i -> o
    s' :: (i, o) -> i -> o



