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
import           Control.Monad (forM_, liftM, liftM3)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust)
import           Safe (headMay)

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

-- | Tells whether or not the Cond is a positive one.
isPosCond :: Cond -> Bool
isPosCond PosCond {} = True
isPosCond PosStr  {} = True
isPosCond PosS    {} = True
isPosCond _          = False
{-# INLINE isPosCond #-}

-- | Tells whether or not the Cond is a negative one.
isNegCond :: Cond -> Bool
isNegCond NegCond {} = True
isNegCond NegStr  {} = True
isNegCond NegS    {} = True
isNegCond _          = False
{-# INLINE isNegCond #-}

-- | Tells whether or not the Cond is a Ncc cond.
isNccCond :: Cond -> Bool
isNccCond NccCond {} = True
isNccCond _          = False
{-# INLINE isNccCond #-}

-- | Puts the Cond into it's canonical form.
canonicalCond :: Env -> Cond -> STM Cond

canonicalCond env (NccCond cs) = liftM NccCond (mapM (canonicalCond env) cs)

canonicalCond _ cond@PosCond {} = return cond
canonicalCond _ cond@NegCond {} = return cond

canonicalCond env (PosStr obj attr val) =
  cctrans env PosCond internSymbol obj attr val
canonicalCond env (PosS obj attr val) =
  cctrans env PosCond sToSymbol obj attr val

canonicalCond env (NegStr obj attr val) =
  cctrans env NegCond internSymbol obj attr val
canonicalCond env (NegS obj attr val) =
  cctrans env NegCond sToSymbol obj attr val
{-# INLINABLE canonicalCond #-}

cctrans :: Env
           -> (Symbol -> Symbol -> Symbol -> Cond)
           -> (Env -> a -> STM Symbol)
           -> a -> a -> a
           -> STM Cond
cctrans env constr trans obj attr val =
  liftM3 constr (trans env obj) (trans env attr) (trans env val)
{-# INLINE cctrans #-}

-- JOIN TESTS

-- | Extracts and returns join tests for the given condition.
joinTestsFromCondition :: Cond -> [Cond] -> [JoinTest]

joinTestsFromCondition (PosCond obj attr val) earlierConds =
  joinTestsFromConditionImpl obj attr val earlierConds

joinTestsFromCondition (NegCond obj attr val) earlierConds =
  joinTestsFromConditionImpl obj attr val earlierConds

joinTestsFromCondition cond@_ _ = error $
  "Join tests may be extracted only from PosCond or NegCond, given" ++
  show cond

-- | The actual implementation for joinTestsFromCondition that
-- uniformy works on symbols.
joinTestsFromConditionImpl :: Symbol
                           -> Symbol
                           -> Symbol
                           -> [Cond]
                           -> [JoinTest]
joinTestsFromConditionImpl obj attr val earlierConds =
  let econds  = indexedPositiveEarlierConds earlierConds
      test1   = joinTestFromField Obj  obj  econds
      test2   = joinTestFromField Attr attr econds
      test3   = joinTestFromField Val  val  econds

      result1 = [fromJust test3 | isJust test3]
      result2 = if isJust test2 then fromJust test2 : result1 else result1
      result3 = if isJust test1 then fromJust test1 : result2 else result2
  in
   result3

-- | Returns a field within the PosCond that exhibits equality with
-- the variable.
variableField :: Cond -> Symbol -> Maybe Field
variableField (PosCond obj attr val) v
  | obj  == v     = Just Obj
  | attr == v     = Just Attr
  | val  == v     = Just Val
  | otherwise     = Nothing
variableField _ _ = error "Only PosConds arguments are allowed."
{-# INLINABLE variableField #-}

type IndexedCond  = (Cond, Int)
data IndexedField = IndexedField !Field !Int

indexedPositiveEarlierConds :: [Cond] -> [IndexedCond]
indexedPositiveEarlierConds earlierConds =
  -- Deliberately indexing from 1 not from 0
  filter positive (zip (reverse earlierConds) [1 ..])
  where
    positive (cond, _) = isPosCond cond
{-# INLINE indexedPositiveEarlierConds #-}

indexedField :: Symbol -> IndexedCond -> Maybe IndexedField
indexedField v (cond, i) =
  case variableField cond v of
    Nothing    -> Nothing
    Just field -> Just (IndexedField field i)
{-# INLINABLE indexedField #-}

joinTestFromField :: Field -> Symbol -> [IndexedCond] -> Maybe JoinTest
joinTestFromField field v earlierConds
  | isVariable v =
      case headMay matches of
        Nothing                 -> Nothing
        Just (IndexedField f i) -> Just (JoinTest field f i)

  -- Non-variables are not taken under the account.
  | otherwise = Nothing
  where
    matches = (map fromJust .
              filter isJust .
              map (indexedField v)) earlierConds
{-# INLINABLE joinTestFromField #-}

-- PROPAGATING CHANGES

-- | Performs a propagation of changes on new nodes creation.
updateNewNodeWithMatchesFromAbove :: Env -> Node -> STM ()
updateNewNodeWithMatchesFromAbove _ _ = undefined
