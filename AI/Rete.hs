{-# LANGUAGE Safe #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-16
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete
    (
      -- * Abstraction
      Rete
    , emptyRete

      -- * Adding Wmes
    , addWme
    , addWmeP

      -- * Adding productions
    , addProd
    , addProdP
    , c
    , var
    , Pred
    , Action

      -- * Variable value access
    , val
    , valE
    , valM

      -- * Strategies
    , StepStrategy
    , breadthFirst
    , depthFirst

      -- * Forward chaining, evaluation
    , forwardChain
    , exec
    , execIO
    , eval

      -- * Predefined actions and tools
    , acompose
    , passAction
    , traceAction
    , traceMsgAction
    )
    where

import AI.Rete.Data
import AI.Rete.Flow
import AI.Rete.Net
import AI.Rete.State
