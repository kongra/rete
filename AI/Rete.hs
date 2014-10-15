{-# LANGUAGE CPP, FlexibleInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-07-21
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This is the interface part for the Rete algorithm.
------------------------------------------------------------------------
module AI.Rete
    (
      c
    , neg
    ) where

import AI.Rete.Data

-- | C is a typeclass that represents creation of positive conditions
-- with various types of arguments.
class C a where c :: a -> a -> a -> Cond

instance C String where c = StringCond
instance C S      where c = SCond
instance C Symbol where c = PositiveCond

-- | Creates either a negative condition or a NCC condition if
-- multiple subconds are passed. The argument must not be empty.
neg :: [Cond] -> Cond
neg [] = error "neg(ation) must have at least 1 subcondition."
neg conds@(x:xs) | null xs   = Neg x
                 | otherwise = Ncc conds
{-# INLINABLE neg #-}
