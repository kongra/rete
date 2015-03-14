{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.State
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-10
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.State
    (
      -- * State manipulation
      viewS
    , setS
    , overS

      -- * Evaluation
    , run
    )
    where

import           AI.Rete.Data
import           Control.Monad (liftM)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import           Data.Hashable (Hashable)
import           Kask.Control.Lens

class State a s where
  -- | Reads a state of the argument in Rete monad.
  viewS :: a -> ReteM s

  -- | Sets a state of the argument in Rete monad.
  setS  :: a -> s -> ReteM ()

-- | Changes a state of the argument using a function in Rete monad.
overS :: State a s => (s -> s) -> a -> ReteM ()
overS f obj = viewS obj >>= setS obj . f
{-# INLINE overS #-}

-- | Runs the computation in the Rete state-monad.
run :: ReteState -> ReteM a -> (a, ReteState)
run = flip S.runState
{-# INLINE run #-}

instance State Rete ReteState where
  viewS _ = S.get
  setS  _ = S.put
  {-# INLINE viewS #-}
  {-# INLINE setS  #-}

instance State Amem AmemState where
  viewS amem   = liftM (lookupState amem . view reteAmemStates) (viewS Rete)
  setS  amem s = viewS Rete >>= setS Rete . over reteAmemStates (Map.insert amem s)
  {-# INLINE viewS #-}
  {-# INLINE setS  #-}

instance State Bmem BmemState where
  viewS bmem   = liftM (lookupState bmem . view reteBmemStates) (viewS Rete)
  setS  bmem s = viewS Rete >>= setS Rete . over reteBmemStates (Map.insert bmem s)
  {-# INLINE viewS #-}
  {-# INLINE setS  #-}

instance State Join JoinState where
  viewS bmem   = liftM (lookupState bmem . view reteJoinStates) (viewS Rete)
  setS  bmem s = viewS Rete >>= setS Rete . over reteJoinStates (Map.insert bmem s)
  {-# INLINE viewS #-}
  {-# INLINE setS  #-}

lookupState :: (Hashable k, Eq k, Show k) => k -> Map.HashMap k v -> v
lookupState k = Map.lookupDefault
                (error ("PANIC (1): STATE NOT FOUND FOR " ++ show k)) k
{-# INLINE lookupState #-}
