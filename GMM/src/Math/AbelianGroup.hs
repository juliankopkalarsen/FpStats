-----------------------------------------------------------------------------
--
-- Module      :  Math.AbelianGroup
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

module Math.AbelianGroup (
    AbelianGroup((<+>), (<->))
) where

-- | Class for the
class AbelianGroup a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a

