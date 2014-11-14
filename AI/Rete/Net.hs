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
import           Control.Monad (forM_, liftM, liftM3, unless, when)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.List (sortBy)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as Seq
import           Kask.Control.Monad (whenM, unlessM, forMM_)
import           Kask.Data.Sequence (removeFirstOccurence)
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
      successors <- newTVar Seq.empty
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

-- NODE CREATION (COMMON PART(S))

-- | Creates and returns a new Node using the arguments. It DOES NOT
-- add the created node to it's parent.children.
newNode :: Env -> Node -> NodeVariant -> STM Node
newNode env parent variant = do
  id'      <- genid env
  children <- newTVar Seq.empty
  return Node { nodeId       = id'
              , nodeParent   = parent
              , nodeChildren = children
              , nodeVariant  = variant }
{-# INLINE newNode #-}

-- BETA MEMORY CREATION

-- | Returns True iff the node is a Bmem node.
isBmem :: Node -> Bool
isBmem node = case nodeVariant node of
  (Bmem {}) -> True
  _         -> False
{-# INLINABLE isBmem #-}

-- | Creates a new β memory or returns a shared one if possible.
buildOrShareBmem :: Env -> Node -> STM Node
buildOrShareBmem env parent = do
  parentChildren <- readTVar (nodeChildren parent)
  let bmems = filter isBmem (toList parentChildren)
  if not (null bmems)
    then return (head bmems)
    else do
      -- Create new node with Bmem variant
      tokens      <- newTVar Set.empty
      allChildren <- newTVar Set.empty
      node <- newNode env parent
              Bmem { nodeTokens      = tokens
                   , bmemAllChildren = allChildren }

      -- Add node to parent's children
      writeTVar (nodeChildren parent) $! (node Seq.<| parentChildren)

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

-- | Puts the Cond into it's canonical form. The canonical form is the
-- one that uses solely Symbol and not Strings nor S-es.
canonicalCond :: Env -> Cond -> STM Cond

canonicalCond env (NccCond cs) =
  liftM NccCond (mapM (canonicalCond env) cs)

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

-- | It is desirable for the Conds of a production to be sorted in
-- such a way that the positive conds come before the negative and the
-- ncc conds. Also the subconditins of a ncc should be sorted in such
-- way. The following procedure does the job.
sortConds :: [Cond] -> [Cond]
sortConds = map processCond . sortBy (flip condsOrdering)
  where
    processCond c = case c of
      (NccCond subconds) -> NccCond (sortConds subconds)
      _                  -> c
{-# INLINE sortConds #-}

condsOrdering :: Cond -> Cond -> Ordering
condsOrdering cond1 cond2
  | isPosCond cond1 = if isPosCond cond2 then EQ else GT
  | isNccCond cond1 = if isNccCond cond2 then EQ else LT
  -- and for the NegativeCond ...
  | otherwise       = if isPosCond cond2 then LT else
                      if isNccCond cond2 then GT else EQ
{-# INLINABLE condsOrdering #-}

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
  let econds  = indexedPositiveConds earlierConds
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

indexedPositiveConds :: [Cond] -> [IndexedCond]
indexedPositiveConds conds =
  filter positive (zip (reverse conds) [0 ..])
  where
    positive (cond, _) = isPosCond cond
{-# INLINE indexedPositiveConds #-}

indexedField :: Symbol -> IndexedCond -> Maybe IndexedField
indexedField v (cond, i) =
  case variableField cond v of
    Nothing    -> Nothing
    Just field -> Just (IndexedField field i)
{-# INLINABLE indexedField #-}

joinTestFromField :: Field -> Symbol -> [IndexedCond] -> Maybe JoinTest
joinTestFromField field v earlierConds
  | isVariable v =
      case headMay (matches earlierConds) of
        Nothing                 -> Nothing
        -- Indices are 0-based. When creating a JoinTest we must
        -- increase it to maintain a required 1-based indexing.
        Just (IndexedField f i) -> Just (JoinTest field f (i+1))

  -- Non-variables do not produce any tests.
  | otherwise = Nothing
  where
    matches = map fromJust . filter isJust . map (indexedField v)
{-# INLINABLE joinTestFromField #-}

-- JOIN NODES

findNearestAncestorWithSameAmem :: Node -> Amem -> Maybe Node
findNearestAncestorWithSameAmem (DummyTopNode {})  _ = Nothing
findNearestAncestorWithSameAmem node amem =
  case nodeVariant node of
    JoinNode {nodeAmem = a}
      -> if amem == a
         then Just node
         else findNearestAncestorWithSameAmem parent amem

    NegativeNode {nodeAmem = a}
      -> if amem == a
         then Just node
         else findNearestAncestorWithSameAmem parent amem

    NccNode {nccPartner = partner}
      -> findNearestAncestorWithSameAmem (nodeParent partner) amem

    -- For all others...
    _ -> findNearestAncestorWithSameAmem parent amem
  where parent = nodeParent node
{-# INLINABLE findNearestAncestorWithSameAmem #-}

isShareableJoinNode :: Amem -> [JoinTest] -> Node -> Bool
isShareableJoinNode amem tests node =
  isJoinNode node
  && amem  == vprop nodeAmem  node
  && tests == vprop joinTests node
{-# INLINABLE isShareableJoinNode #-}

buildOrShareJoinNode :: Env -> Node -> Amem -> [JoinTest] -> STM Node
buildOrShareJoinNode env parent amem tests = do
  -- parent is always a β-memory, so below it's safe
  allParentChildren <- rvprop bmemAllChildren parent
  let matchingOneOf = headMay
                      . filter (isShareableJoinNode amem tests)
                      . Set.toList
  case matchingOneOf allParentChildren of
    Just node -> return node
    Nothing   -> do
      -- Establish the unlinking stuff ...
      unlinkRight <- nullTSet (vprop nodeTokens parent)
      ru          <- newTVar unlinkRight
      -- ... unlinking left only if the right ul. was not applied.
      unlinkLeft'    <- nullTSet (amemWmes amem)
      let unlinkLeft = not unlinkRight && unlinkLeft'
      lu             <- newTVar unlinkLeft

      -- Create new node with JoinNode variant
      let ancestor = findNearestAncestorWithSameAmem parent amem
      node <- newNode env parent
              JoinNode { nodeAmem                    = amem
                       , nearestAncestorWithSameAmem = ancestor
                       , joinTests                   = tests
                       , leftUnlinked                = lu
                       , rightUnlinked               = ru }

      -- Add node to parent.allChildren
      writeTVar (vprop bmemAllChildren parent) $!
        Set.insert node allParentChildren

      -- Increment amem.reference-count
      modifyTVar' (amemReferenceCount amem) (+1)

      unless unlinkRight $
        -- Insert node at the head of amem.successors
        modifyTVar' (amemSuccessors amem) (node Seq.<|)

      unless unlinkLeft $
        -- Add node (to the head) of parent.children
        modifyTVar' (nodeChildren parent) (node Seq.<|)

      return node
{-# INLINABLE buildOrShareJoinNode #-}

-- NEGATIVE NODES

isShareableNegativeNode :: Amem -> [JoinTest] -> Node -> Bool
isShareableNegativeNode amem tests node =
  isNegativeNode node
  && amem  == vprop nodeAmem  node
  && tests == vprop joinTests node
{-# INLINABLE isShareableNegativeNode #-}

buildOrShareNegativeNode :: Env -> Node -> Amem -> [JoinTest] -> STM Node
buildOrShareNegativeNode env parent amem tests = do
  parentChildren <- readTVar (nodeChildren parent)
  let matchingOneOf = headMay
                      . filter (isShareableNegativeNode amem tests)
                      . toList
  case matchingOneOf parentChildren of
    Just node -> return node
    Nothing   -> do
      -- Create new node with JoinNode variant
      let ancestor = findNearestAncestorWithSameAmem parent amem
      toks <- newTVar Set.empty
      ru   <- newTVar False
      node <- newNode env parent
              NegativeNode { nodeTokens                  = toks
                           , nodeAmem                    = amem
                           , nearestAncestorWithSameAmem = ancestor
                           , joinTests                   = tests
                           , rightUnlinked               = ru }

      -- Insert node at the head of parent.children
      writeTVar (nodeChildren parent) $! node Seq.<| parentChildren

      -- Insert node at the head of amem.successors
      modifyTVar' (amemSuccessors amem) (node Seq.<|)

      -- Increment amem.reference-count
      modifyTVar' (amemReferenceCount amem) (+1)

      updateNewNodeWithMatchesFromAbove env node

      -- Right unlink, but only after updating from above.
      whenM (nullTSet (vprop nodeTokens node)) $
        rightUnlink node amem

      return node
{-# INLINABLE buildOrShareNegativeNode #-}

-- NCC NODES CREATION

isShareableNccNode :: Node -> Node -> Bool
isShareableNccNode bottomOfSubnetwork node =
  isNccNode node
  && nodeParent (vprop nccPartner node) == bottomOfSubnetwork
{-# INLINABLE isShareableNccNode #-}

buildOrShareNccNodes :: Env
                     -> Node      -- ^ parent
                     -> Cond      -- ^ the NccCond
                     -> [Cond]    -- ^ earlier conds
                     -> STM Node  -- ^ the Ncc node
buildOrShareNccNodes env parent (NccCond subconds) earlierConds = do
  bottomOfSubnetwork <- buildOrShareNetworkForConditions
                          env parent subconds earlierConds
  parentChildren     <- readTVar (nodeChildren parent)
  let matchingOneOf = headMay
                      . filter (isShareableNccNode bottomOfSubnetwork)
                      . toList
  case matchingOneOf parentChildren of
    Just node -> return node
    Nothing   -> do
      newResultBuff <- newTVar Set.empty
      nccNode <- newTVar Nothing
      partner <- newNode env bottomOfSubnetwork
                 NccPartner { nccPartnerNccNode          = nccNode
                            , nccPartnerNumberOfConjucts = length subconds
                            , nccPartnerNewResultBuffer  = newResultBuff }

      toks <- newTVar Set.empty
      new  <- newNode env parent
              NccNode { nodeTokens = toks
                      , nccPartner = partner }

      -- Bind new to the partner.
      writeTVar (vprop nccPartnerNccNode partner) $! Just new

      -- Insert new at the tail of parent.children (so that the
      -- subnetwork comes first).
      writeTVar (nodeChildren parent) $! parentChildren Seq.|> new

        -- Insert partner at the head of bottomOfSubnetwork.children.
      modifyTVar' (nodeChildren bottomOfSubnetwork) (partner Seq.<|)

      -- Note: we have to inform NCC node of existing matches before
      -- informing the partner, otherwise lots of matches would all
      -- get mixed together in the new-result-buffer.
      updateNewNodeWithMatchesFromAbove env new
      updateNewNodeWithMatchesFromAbove env partner

      return new

buildOrShareNccNodes _ _ _ _ =
  error "Illegal cond - only NccConds accepted."
{-# INLINABLE buildOrShareNccNodes #-}

-- BUILDING THE NETWORK

buildOrShareNetworkForConditions :: Env
                                 -> Node      -- ^ parent
                                 -> [Cond]    -- ^ conds
                                 -> [Cond]    -- ^ earlier conds
                                 -> STM Node  -- ^ the resulting node

-- When no conds, simply return parent
buildOrShareNetworkForConditions _ parent [] _ = return parent

buildOrShareNetworkForConditions env parent (c:cs) earlierConds = do
  let updatedEarlierConds = earlierConds ++ [c]
  case c of
    (PosCond obj attr val) -> do
      currentParent <- if parent == envDummyTopNode env
                       -- No need to build a β-memory before building
                       -- the first JoinNode, because DummyTopNode is
                       -- β-memory like.
                       then return parent
                       else buildOrShareBmem env parent
      let tests   =  joinTestsFromCondition c earlierConds
      amem        <- buildOrShareAmem env obj attr val
      currentNode <- buildOrShareJoinNode env currentParent amem tests
      buildOrShareNetworkForConditions env currentNode
        cs updatedEarlierConds

    (NegCond obj attr val) -> do
      let tests   =  joinTestsFromCondition c earlierConds
      amem        <- buildOrShareAmem env obj attr val
      currentNode <- buildOrShareNegativeNode env parent amem tests
      buildOrShareNetworkForConditions env currentNode
        cs updatedEarlierConds

    NccCond {} -> do
      currentNode <- buildOrShareNccNodes env parent c earlierConds
      buildOrShareNetworkForConditions env currentNode
        cs updatedEarlierConds

    _ -> error ("Illegal c(ond) " ++ show c)
{-# INLINABLE buildOrShareNetworkForConditions #-}

-- PROPAGATING CHANGES

-- | Performs a propagation of changes on new nodes creation.
updateNewNodeWithMatchesFromAbove :: Env -> Node -> STM ()

updateNewNodeWithMatchesFromAbove _ DummyTopNode {} =
  error "Illegal attempt to update from above the DummyTopNode."

updateNewNodeWithMatchesFromAbove env node = do
  let parent = nodeParent node
  case nodeVariant parent of
    Bmem {nodeTokens = toks} -> forMM_ (toListT toks) $ \tok ->
      leftActivate env tok Nothing node

    DTN {} -> leftActivate env (envDummyTopToken env) Nothing node

    JoinNode {} -> do
      savedChildren <- readTVar (nodeChildren parent)
      writeTVar (nodeChildren parent) (Seq.singleton node)
      forMM_ (toListT (amemWmes (vprop nodeAmem parent))) $ \wme ->
        rightActivate env wme parent
      writeTVar (nodeChildren parent) savedChildren

    NegativeNode {} ->
      forMM_ (toListT (vprop nodeTokens parent)) $ \tok ->
        whenM (nullTSet (tokNegJoinResults tok)) $
          leftActivate env tok Nothing node

    NccNode {} ->
      forMM_ (toListT (vprop nodeTokens parent)) $ \tok ->
        whenM (nullTSet (tokNccResults tok)) $
          leftActivate env tok Nothing node

    _ -> error "Illegal parent when updateNewNodeWithMatchesFromAbove!"
{-# INLINABLE updateNewNodeWithMatchesFromAbove #-}

-- ADDING PRODUCTIONS

-- | Adds a new production and returns its Node (having PNode variant).
addProduction :: Env
              -> [Cond]        -- ^ Conditions
              -> Action        -- ^ Then action
              -> Maybe Action  -- ^ Revoke action
              -> STM Node      -- ^ Rete node for the production
addProduction env conds action revokeAction = do
  -- Conds must be turned into their canonical form and ordered to put
  -- positive conds in front.
  cconds <- liftM sortConds (mapM (canonicalCond env) conds)
  parent <- buildOrShareNetworkForConditions env
              (envDummyTopNode env) cconds []

  let vbindings = variableBindingsForConds cconds
  toks <- newTVar Set.empty
  node <- newNode env parent
          PNode { nodeTokens            = toks
                , pnodeAction           = action
                , pnodeRevokeAction     = revokeAction
                , pnodeVariableBindings = vbindings}

  -- Add node to parent's children.
  modifyTVar' (nodeChildren parent) (node Seq.<|)

  -- Add the node to the registry in the env
  modifyTVar' (envProductions env) (Set.insert node)

  -- Update with matches from above.
  updateNewNodeWithMatchesFromAbove env node

  -- Done.
  return node
{-# INLINABLE addProduction #-}

-- | Works like 'addProduction' inside an action (of a production).
addProductionA :: Actx -> [Cond] -> Action -> Maybe Action -> STM Node
addProductionA actx = addProduction (actxEnv actx)
{-# INLINE addProductionA #-}

-- | Configures variable bindings for the conditions of a production.
variableBindingsForConds :: [Cond] -> VariableBindings
variableBindingsForConds conds = loop Map.empty (indexedPositiveConds conds)
  where
    loop result []                               = result
    loop result ((PosCond obj attr val, i) : cs) =
      loop result3 cs
      where
        result1 = variableBindingsForCond obj  Obj  i result
        result2 = variableBindingsForCond attr Attr i result1
        result3 = variableBindingsForCond val  Val  i result2
    loop _ _ = error "Only PosConds allowed here."
{-# INLINE variableBindingsForConds #-}

variableBindingsForCond :: Symbol
                        -> Field
                        -> Int               -- ^ distance
                        -> VariableBindings  -- ^ bindings so far
                        -> VariableBindings  -- ^ updated bindings
variableBindingsForCond s f dist bindings =
  case s of
    -- For normal symbols leave bindings untouched
    Symbol   {} -> bindings
    -- For variables avoid overriding existing bindings
    Variable {} -> if Map.member s bindings then bindings
                   else Map.insert s (SymbolLocation f dist) bindings
{-# INLINE variableBindingsForCond #-}

-- ACCESSING INFORMATION IN ACTIONS

-- | Returns the value of a variable inside an action or Nothing if
-- the symbol was not recognized (no such variable, not a variable
-- symbol).
valA :: Actx -> Symbol -> Maybe Symbol
valA Actx {actxNode = node, actxWmes = wmes} s =
  case Map.lookup s (vprop pnodeVariableBindings node) of
    Nothing                      -> Nothing
    Just (SymbolLocation f dist) ->
      Just (fieldValue f (fromJust (wmes !! dist)))
{-# INLINABLE valA #-}

-- DELETING NODES

hasDeletableNodeTokens :: Node -> Bool
hasDeletableNodeTokens node = case nodeVariant node of
  Bmem         {} -> True
  NegativeNode {} -> True
  NccNode      {} -> True
  PNode        {} -> True  -- Extend this with new variants if necessary.
  _               -> False
{-# INLINE hasDeletableNodeTokens #-}

hasNodeAmem :: Node -> Bool
hasNodeAmem node = case nodeVariant node of
  JoinNode {}     -> True
  NegativeNode {} -> True
  _               -> False
{-# INLINE hasNodeAmem #-}

notLeftUnlinked :: Node -> STM Bool
notLeftUnlinked node = case nodeVariant node of
  JoinNode {} -> liftM not (rvprop leftUnlinked node)
  _           -> return True
{-# INLINE notLeftUnlinked #-}

deleteNodeAndAnyUnusedAncestor :: Env -> Node -> STM ()
deleteNodeAndAnyUnusedAncestor env node@Node {} = do
  -- Please notice, we operate in Node here, but not in DummyTopNode.

  -- For NccNodes, delete the partner too.
  when (isNccNode node) $
    deleteNodeAndAnyUnusedAncestor env (vprop nccPartner node)

  -- Clean up any tokens the node contains.
  when (hasDeletableNodeTokens node) $
    forMM_ (toListT (vprop nodeTokens node)) $ \tok ->
      -- When removing the token we remove it both from its wme and
      -- its parent (tok).
      deleteTokenAndDescendents env True True tok

  -- Do special cleanup in NccPartner.
  when (isNccPartner node) $
    forMM_ (toListT (vprop nccPartnerNewResultBuffer node)) $ \tok ->
      -- When removing the token we remove it both from its wme and
      -- its parent (tok).
      deleteTokenAndDescendents env True True tok

  -- Deal with the α memory.
  when (hasNodeAmem node) $
    unlessM (isRightUnlinked node) $ do
      -- Remove node from node.amem.successors
      let amem   = vprop nodeAmem node
      modifyTVar' (amemSuccessors amem) (removeFirstOccurence node)
      count <- readTVar (amemReferenceCount amem)
      -- Decrement the count
      writeTVar (amemReferenceCount amem) $! count - 1
      -- If the updated count is 0 (i.e. original 1), delete amem
      when (count == 1) (deleteAmem env amem)

  -- Deal with the parent.
  let parent = nodeParent node
  whenM (notLeftUnlinked node) $
    -- Remove node from parent.children
    modifyTVar' (nodeChildren parent) (removeFirstOccurence node)

  when (isJoinNode node) $
    -- A parent of a join node is either a Bmem or DTN. Remove node
    -- from parent.bmemAllChildren.
    modifyTVar' (vprop bmemAllChildren parent) (Set.delete node)

  if isBmem parent then
    -- For Bmems we have to check both children and allChildren
    (do emptyAll      <- nullTSet (vprop bmemAllChildren parent)
        emptyChildren <- liftM Seq.null (readTVar (nodeChildren parent))
        when (emptyChildren && emptyAll) $
          deleteNodeAndAnyUnusedAncestor env parent)
    else
      whenM (liftM Seq.null (readTVar (nodeChildren parent))) $
        deleteNodeAndAnyUnusedAncestor env parent

-- For DummyTopNode we don't do anything
deleteNodeAndAnyUnusedAncestor _ DummyTopNode {} = return ()
{-# NOINLINE deleteNodeAndAnyUnusedAncestor #-}

deleteAmem :: Env -> Amem -> STM ()
deleteAmem
  Env  { envAmems = amems }
  Amem { amemObj  = obj
       , amemAttr = attr
       , amemVal  = val }
  = modifyTVar' amems (Map.delete (WmeKey obj attr val))
{-# INLINE deleteAmem #-}

-- REMOVING PRODUCTIONS

-- | Removes a production given as a production Node (the one with
-- PNode variant). Returns True on successful removal and False, when
-- the production was already removed.
removeProduction :: Env -> Node -> STM Bool
removeProduction env@Env { envProductions = prods } node = do
  prodset <- readTVar prods
  if Set.member node prodset then
    (do writeTVar prods $! Set.delete node prodset
        deleteNodeAndAnyUnusedAncestor env node
        return True)

    else return False
{-# INLINABLE removeProduction #-}

-- | Works like removeProduction inside an action (of a production).
removeProductionA :: Actx -> Node -> STM Bool
removeProductionA actx = removeProduction (actxEnv actx)
{-# INLINE removeProductionA #-}
