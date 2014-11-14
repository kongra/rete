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
      -- * 'Env'ironment
      Env
    , createEnv

      -- * 'Symbol's
    , S (..)

      -- * 'Cond'itions
    , Cond
    , c
    , neg
    , ncc

      -- * 'Action's
    , Action
    , Actx

      -- * Adding/removing 'Wme's
    , addWme
    , addWmeA
    , removeWme
    , removeWmeA

      -- * Adding/removing productions
    , addProduction
    , addProductionA
    , removeProduction
    , removeProductionA

      -- * Accessing information in actions
    , valA
    )
    where

import AI.Rete.Algo
import AI.Rete.Data
import AI.Rete.Net

class C a where
  -- | Creates a positive condition.
  c :: a  -- ^ Obj
    -> a  -- ^ Attr
    -> a  -- ^ Val
    -> Cond

instance C String where c = PosStr
instance C S      where c = PosS
instance C Symbol where c = PosCond

class Neg a where
  -- | Creates a negative condition.
  neg :: a  -- ^ Obj
      -> a  -- ^ Attr
      -> a  -- ^ Val
      -> Cond

instance Neg String where neg = NegStr
instance Neg S      where neg = NegS
instance Neg Symbol where neg = NegCond

-- | Creates a negated conjunction condition.
ncc :: [Cond] -> Cond
ncc []      = error "Ncc must have at least 2 subconditions, [] given."
ncc [_]     = error "Ncc must have at least 2 subconditions, 1 given."
ncc conds@_ = NccCond conds
{-# INLINABLE ncc #-}
