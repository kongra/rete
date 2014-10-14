{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Net
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-03
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- This module contains routines for managing the Rete network
-- structure.
------------------------------------------------------------------------
module AI.Rete.Net where

import           AI.Rete.Algo
import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Monad (forM_)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- | Tells whether or not the Symbols is a Variable.
isVariable :: Symbol -> Bool
isVariable (Variable _ _) = True
isVariable _              = False
{-# INLINE isVariable #-}

-- ALPHA MEMORY CREATION

-- | Reaches for an existing alpha memory for the given symbols or
-- creates a new one.
buildOrShareAmem :: Env -> Symbol -> Symbol -> Symbol -> STM Amem
buildOrShareAmem env obj attr val = do
  let obj'  = if isVariable obj  then wildcardSymbol else obj
      attr' = if isVariable attr then wildcardSymbol else attr
      val'  = if isVariable val  then wildcardSymbol else val

  amems <- readTVar (envAmems env)
  let k = WmeKey obj' attr' val'
  case Map.lookup k amems of
    Just amem -> return amem  -- Happily found.
    Nothing   -> do
      -- No amem found, let's create one.
      successors <- newTVar []
      refCount   <- newTVar 0

      wmes       <- newTVar Set.empty
      wmesByObj  <- newTVar Map.empty
      wmesByAttr <- newTVar Map.empty
      wmesByVal  <- newTVar Map.empty

      let amem = Amem { amemObj            = obj'
                      , amemAttr           = attr'
                      , amemVal            = val'
                      , amemSuccessors     = successors
                      , amemReferenceCount = refCount
                      , amemWmes           = wmes
                      , amemWmesByObj      = wmesByObj
                      , amemWmesByAttr     = wmesByAttr
                      , amemWmesByVal      = wmesByVal }

      -- Put amem into the env registry of Amems.
      writeTVar (envAmems env) $! Map.insert k amem amems

      activateAmemOnCreation env amem obj' attr' val'

      -- Now we're done.
      return amem
{-# INLINABLE buildOrShareAmem #-}

-- | A simplified, more effective version of amem activation that
-- takes place on the amem creation. No successors activation here, cause
-- no successors present.
activateAmemOnCreation :: Env -> Amem -> Symbol -> Symbol -> Symbol -> STM ()
activateAmemOnCreation env amem obj attr val = do
  byObjIndex  <- readTVar (envWmesByObj  env)
  byAttrIndex <- readTVar (envWmesByAttr env)
  byValIndex  <- readTVar (envWmesByVal  env)

  let wmesMatchingByObj  = Map.lookupDefault Set.empty obj  byObjIndex
      wmesMatchingByAttr = Map.lookupDefault Set.empty attr byAttrIndex
      wmesMatchingByVal  = Map.lookupDefault Set.empty val  byValIndex
      wmesMatching       = wmesMatchingByObj  `Set.intersection`
                           wmesMatchingByAttr `Set.intersection`
                           wmesMatchingByVal

  -- Put all matching wmes into the amem
  writeTVar (amemWmes amem) $!  wmesMatching

  -- Iteratively work on every wme
  forM_ (Set.toList wmesMatching) $ \wme -> do
    -- put amem to wme registry of Amems
    modifyTVar' (wmeAmems wme) (amem:)

    -- put wme into amem indexes
    modifyTVar' (amemWmesByObj  amem) (wmesIndexInsert (wmeObj  wme) wme)
    modifyTVar' (amemWmesByAttr amem) (wmesIndexInsert (wmeAttr wme) wme)
    modifyTVar' (amemWmesByVal  amem) (wmesIndexInsert (wmeVal  wme) wme)
{-# INLINE activateAmemOnCreation #-}

-- BETA MEMORY CREATION

-- | Returns True iff the node is a Bmem node.
isBmem :: Node -> Bool
isBmem node = case nodeVariant node of
  (Bmem {}) -> True
  _         -> False
{-# INLINABLE isBmem #-}

-- | Creates a new beta memory or returns a shared one if possible.
buildOrShareBmem :: Env -> Node -> STM Node
buildOrShareBmem env parent = do
  parentChildren <- readTVar (nodeChildren parent)
  let bmems = filter isBmem parentChildren
  if not (null bmems)
    then return (head bmems)
    else do
      -- Create new Bmem variant
      tokens      <- newTVar Set.empty
      allChildren <- newTVar Set.empty
      let variant = Bmem { nodeTokens      = tokens
                         , bmemAllChildren = allChildren }
      -- Create new node
      id'      <- genid env
      children <- newTVar []
      let node = Node { nodeId       = id'
                      , nodeParent   = parent
                      , nodeChildren = children
                      , nodeVariant  = variant }

      -- Add node to parent's children
      writeTVar (nodeChildren parent) $! (node:parentChildren)

      -- Update with matches from above.
      updateNewNodeWithMatchesFromAbove env node

      -- We're done.
      return node
{-# INLINABLE buildOrShareBmem #-}

-- JOIN TESTS

-- c1 :: Cond
-- c1 = C "Ala" "ma" "psa"

-- c2 :: Cond
-- c2 = CS

-- | Performs a propagation of changes on new nodes creation.
updateNewNodeWithMatchesFromAbove :: Env -> Node -> STM ()
updateNewNodeWithMatchesFromAbove _ _ = undefined
