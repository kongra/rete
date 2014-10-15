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
    , ncc
    ) where

import AI.Rete.Data

-- | C is a typeclass that represents creation of positive conditions
-- with various types of arguments.
class C a where c :: a -> a -> a -> Cond

instance C String where c = PosStr
instance C S      where c = PosS
instance C Symbol where c = PosCond

class Neg a where neg :: a -> a -> a -> Cond
instance Neg String where neg = NegStr
instance Neg S      where neg = NegS
instance Neg Symbol where neg = NegCond

-- | Creates a negated conjunction condition.
ncc :: [Cond] -> Cond
ncc []      = error "Ncc must have at least 2 subconditions, [] given."
ncc [_]     = error "Ncc must have at least 2 subconditions, 1 given."
ncc conds@_ = Ncc conds
{-# INLINABLE ncc #-}
