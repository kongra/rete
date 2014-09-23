{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Algo
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-07-23
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
------------------------------------------------------------------------
module AI.Rete.Algo where

import Data.Maybe (isJust, fromJust)
import Data.Typeable
import Control.Monad (when, unless, liftM, liftM2, forM_)
import Control.Exception (Exception)
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

import AI.Rete.Data

-- MISC. UTILS

-- | Inserts y before first occurence of x within the list. When y
-- does not occur in the list, returns the original list. Thanks to:
-- http://stackoverflow.com/questions/14774153/
--      a-function-that-inserts-element-y-before-first-occurrence-of-x
insertBefore :: Eq a =>
                a       -- ^ y - the element to insert
                -> a    -- ^ x - the list member
                -> [a]  -- ^ the list
                -> [a]
insertBefore _ _ [] = []
insertBefore y x (a:as)
  | a == x    = y:a:as
  | otherwise = a : insertBefore y x as
{-# INLINABLE insertBefore #-}

-- ENVIRONMENT, GENERATING IDS

-- | Creates a new Env
createEnv :: STM Env
createEnv = do
  id'           <- newTVar 0
  symbols       <- newTVar Map.empty
  workingMemory <- newTVar Map.empty
  amemsRegistry <- newTVar Map.empty
  return (Env id' symbols workingMemory amemsRegistry)
{-# INLINABLE createEnv #-}

-- | Generates a new ID
genid :: Env -> STM ID
genid Env {envId = eid} = do
  recent <- readTVar eid
  when (recent == maxBound)
    -- hopefully not in a reasonable time
    (throwSTM IDOverflow)

  let new = recent + 1
  writeTVar eid $! new
  return new
{-# INLINABLE genid #-}

data IDOverflow = IDOverflow deriving (Show, Typeable)
instance Exception IDOverflow

-- SYMBOLS, INTERNING

emptySymbol :: Symbol
emptySymbol = Symbol (-1) ""

emptyVariable :: Symbol
emptyVariable = Variable (-2) "?"

wildcardSymbol :: Symbol
wildcardSymbol = Symbol (-3) "* (wildcard symbol)"

-- | Interns a symbol
internSymbol :: Env -> String -> STM Symbol
internSymbol _ "" = return emptySymbol

internSymbol env name@[x]
  | x == '?'  = return emptyVariable
  | otherwise = doInternSymbol env name Symbol

internSymbol env name@(x:_)
  | x == '?'  = doInternSymbol env name Variable
  | otherwise = doInternSymbol env name Symbol
{-# INLINABLE internSymbol #-}

type SymbolConstructor = ID -> String -> Symbol

doInternSymbol :: Env -> String -> SymbolConstructor -> STM Symbol
doInternSymbol env name constr = do
  registry <- readTVar (envSymbolsRegistry env)
  case Map.lookup name registry of
    Just s  -> return s
    Nothing -> do
      id' <- genid env
      let s = constr id' name
      modifyTVar' (envSymbolsRegistry env) (Map.insert name s)
      return s
{-# INLINABLE doInternSymbol #-}

-- α MEMORY, ACTIVATION

-- | Creates an updated version of the α memory index by putting a new
-- wme under the key s.
amemIndexInsert :: Symbol -> WME -> AmemIndex -> AmemIndex
amemIndexInsert s wme ai = Map.insert s newSet ai
  where oldSet = Map.lookupDefault Set.empty s ai
        newSet = Set.insert wme oldSet
{-# INLINABLE amemIndexInsert #-}

-- | Activates the α memory
activateAmem :: Env -> Amem -> WME -> STM ()
activateAmem env amem wme = do
  -- put amem to wme registry of Amems
  modifyTVar' (wmeAmems wme) (amem:)

  -- put wme into the amem
  modifyTVar' (amemWmes amem) (Set.insert wme)

  -- put wme into amem indexes
  modifyTVar' (amemWmesByObj  amem) (amemIndexInsert (wmeObj  wme) wme)
  modifyTVar' (amemWmesByAttr amem) (amemIndexInsert (wmeAttr wme) wme)
  modifyTVar' (amemWmesByVal  amem) (amemIndexInsert (wmeVal  wme) wme)

  -- increase the amem wmes count
  modifyTVar' (amemWmesCount amem) (+1)

  -- right-activate all successors (children)
  successors <- readTVar (amemSuccessors amem)
  mapM_ (rightActivate env wme) successors
{-# INLINABLE activateAmem #-}

-- | Propagates the wme into the corresponding α memories and further
-- down the network
feedAmems :: Env -> WME -> Symbol -> Symbol -> Symbol -> STM ()
feedAmems  env wme obj attr val = do
  feedAmem env wme obj attr           val
  feedAmem env wme obj attr           wildcardSymbol
  feedAmem env wme obj wildcardSymbol val
  feedAmem env wme obj wildcardSymbol wildcardSymbol

  feedAmem env wme wildcardSymbol attr           val
  feedAmem env wme wildcardSymbol attr           wildcardSymbol
  feedAmem env wme wildcardSymbol wildcardSymbol val
  feedAmem env wme wildcardSymbol wildcardSymbol wildcardSymbol
{-# INLINABLE feedAmems #-}

feedAmem :: Env -> WME -> Symbol -> Symbol -> Symbol -> STM ()
feedAmem env wme obj attr val = do
  amems <- readTVar (envAmems env)
  case Map.lookup (WMEKey obj attr val) amems of
    Just amem -> activateAmem env amem wme
    Nothing   -> return ()
{-# INLINABLE feedAmem #-}

-- WMES, ADDING

-- | Adds the fact represented by the symbols (encoded in Strings) to
-- the working memory and propagates the change downwards the Rete
-- network. Returns the corresponding WME. If the fact was already
-- present, nothing happens.
addWme :: Env -> String -> String -> String -> STM (Maybe WME)
addWme env obj attr val = do
  obj'  <- internSymbol env obj
  attr' <- internSymbol env attr
  val'  <- internSymbol env val

  workingMemory <- readTVar (envWorkingMemory env)
  let k = WMEKey obj' attr' val'

  if Map.member k workingMemory
    then return Nothing  -- Already present, do nothing.
    else do
      wme <- createWme env obj' attr' val'
      -- Put wme to the Working Memory ...
      writeTVar (envWorkingMemory env) (Map.insert k wme workingMemory)
      -- ... and propagate the addition.
      feedAmems env wme obj' attr' val'
      return (Just wme)
{-# INLINABLE addWme #-}

-- | Creates an empty WME
createWme :: Env -> Symbol -> Symbol -> Symbol -> STM WME
createWme env obj attr val = do
  id'       <- genid env
  amems     <- newTVar []
  toks      <- newTVar Set.empty
  njResults <- newTVar Set.empty

  return WME { wmeId             = id'
             , wmeObj            = obj
             , wmeAttr           = attr
             , wmeVal            = val
             , wmeAmems          = amems
             , wmeTokens         = toks
             , wmeNegJoinResults = njResults}
{-# INLINE createWme #-}

-- RIGHT ACTIVATION DISPATCH

-- | Right-activates the node with the passed wme
rightActivate :: Env -> WME -> ReteNode -> STM ()
rightActivate env wme node = do
  let variant = reteNodeVariant node
  case variant of
    JoinNode     {} -> rightActivateJoinNode     env wme node variant
    NegativeNode {} -> rightActivateNegativeNode env wme node variant
    _               -> error "Illegal ReteNode variant used to rightActivate"
{-# INLINABLE rightActivate #-}

-- LEFT ACTIVATION DISPATCH

-- | Left-activates the node
leftActivate :: Env -> Token -> Maybe WME -> ReteNode -> STM ()
leftActivate env tok wme node = do
  let variant = reteNodeVariant node
  case variant of
    Bmem           {} -> leftActivateBmem         env tok wme node variant
    JoinNode       {} -> leftActivateJoinNode     env tok wme node variant
    NegativeNode   {} -> leftActivateNegativeNode env tok wme node variant
    NCCNode        {} -> leftActivateNCCNode      env tok wme node variant
    NCCPartner     {} -> leftActivateNCCPartner   env tok wme node variant
    PNode          {} -> leftActivatePNode        env tok wme node variant
{-# INLINABLE leftActivate #-}

-- β MEMORIES, TOKENS

leftActivateBmem ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivateBmem env tok wme node variant = do
  newTok <- makeAndInsertToken env tok wme node variant

  -- Left-activate children (solely JoinNodes, do not pass any wme)
  children <- readTVar (reteNodeChildren node)
  mapM_ (leftActivate env newTok Nothing) children
{-# INLINABLE leftActivateBmem #-}

makeToken :: Env -> Token -> Maybe WME -> ReteNode -> STM Token
makeToken env parentTok wme node = do
  id'        <- genid env
  children   <- newTVar Set.empty
  njResults  <- newTVar Set.empty
  nccResults <- newTVar Set.empty

  let tok = Token { tokId             = id'
                  , tokParent         = Just parentTok
                  , tokWme            = wme
                  , tokNode           = node
                  , tokChildren       = children
                  , tokNegJoinResults = njResults
                  , tokNccResults     = nccResults
                  , tokOwner          = Nothing}

  -- Add tok to parentTok.children (for tree-based removal)
  modifyTVar' (tokChildren parentTok) (Set.insert tok)

  when (isJust wme)
    -- Add tok to wme.tokens (for tree-based-removal)
    (modifyTVar' (wmeTokens (fromJust wme)) (Set.insert tok))

  return tok
{-# INLINABLE makeToken #-}

-- | Creates a token and adds it to the node.
makeAndInsertToken ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM Token
makeAndInsertToken env tok wme node variant = do
  newTok <- makeToken env tok wme node
  modifyTVar' (nodeTokens variant) (Set.insert newTok)
  modifyTVar' (nodeTokensCount variant) (+1)
  return newTok
{-# INLINABLE makeAndInsertToken #-}

-- | Returns a sequence of wmes within the token. Every wme wrapped in
-- a Maybe instance due to the fact that some tokens do not carry on
-- any wme.
tokenWmes :: Token -> [Maybe WME]
tokenWmes = map tokWme'
            . takeWhile isJust  -- to avoid going into Nothings
            . iterate parentTok
            . Just
  where
    parentTok (Just tok) = tokParent tok
    parentTok Nothing    = Nothing
    tokWme' tok = tokWme (fromJust tok)  -- safely, we skip Nothing tokens
{-# INLINABLE tokenWmes #-}

-- UNINDEXED JOIN TESTS

-- | Performs the join tests not using any kind of indexing. Useful
-- while right-activation, when the α memory passes a single wme, so
-- there is no use of the α memory indexing.
performJoinTests :: [JoinTest] -> Token -> WME -> Bool
performJoinTests tests tok wme = all (passJoinTest tok wme) tests
{-# INLINE performJoinTests #-}

passJoinTest :: Token -> WME -> JoinTest -> Bool
passJoinTest tok wme test = value1 == value2
  where
    value1    = fieldValue (joinTestField1 test) wme
    -- We feel safe to coerce to Just wme2 below because of the way
    -- the tests are constructed.
    Just wme2 = tokenWmes tok !! joinTestDistance test
    value2    = fieldValue (joinTestField2 test) wme2
{-# INLINABLE passJoinTest #-}

-- | Reaches a value of the wme in the specified field.
fieldValue :: Field -> WME -> Symbol
fieldValue Obj  = wmeObj
fieldValue Attr = wmeAttr
fieldValue Val  = wmeVal
{-# INLINE fieldValue #-}

-- INDEXED JOIN TESTS (USING α MEMORY INDEXES)

-- | Matches a token to wmes in an α memory using the α memory indexes.
matchingAmemWmes :: [JoinTest] -> Token -> Amem -> STM [WME]
matchingAmemWmes [] _ amem =
  -- When no tests specified, we simply take all wmes from the α memory
  liftM Set.toList (readTVar (amemWmes amem))

matchingAmemWmes tests tok amem = do
  -- When at least one test specified ...
  let wmes = tokenWmes tok
      (s:sets) = map (amemWmesForTest wmes amem) tests
  liftM Set.toList (foldr (liftM2 Set.intersection) s sets)
{-# INLINABLE matchingAmemWmes #-}

-- | Uses proper indexes of the α memory to return a set of wmes
-- matching the token (represented as a sequence of Maybe WME) wrt the
-- current test.
amemWmesForTest :: [Maybe WME] -> Amem -> JoinTest -> STM (Set.HashSet WME)
amemWmesForTest wmes amem test = do
  index <- amemIndexForField (joinTestField1 test) amem
  return (Map.lookupDefault Set.empty value index)
  where
    Just wme = wmes !! joinTestDistance test
    value = fieldValue (joinTestField2 test) wme

    amemIndexForField Obj  = readTVar . amemWmesByObj
    amemIndexForField Attr = readTVar . amemWmesByAttr
    amemIndexForField Val  = readTVar . amemWmesByVal
{-# INLINABLE amemWmesForTest #-}

-- JOIN NODES

leftActivateJoinNode ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivateJoinNode env tok _ node variant = do
  let amem        = nodeAmem variant
      Just parent = reteNodeParent node
      -- safely above, JoinNodes always have parents

  wmesCount <- readTVar (amemWmesCount amem)

  -- When the parent just became non-empty (equivalently testing for
  -- the right-unlinked flag) ...
  rightUnlinked' <- readTVar (rightUnlinked variant)
  when rightUnlinked' $ do
    relinkToAlphaMemory node variant
    writeTVar (rightUnlinked variant) False

    -- When amem is empty ...
    when (wmesCount == 0) $ do
      -- ... left-unlink this node
      modifyTVar' (reteNodeChildren parent) (filter (/= node))
      writeTVar   (leftUnlinked variant) True

  when (wmesCount /= 0) $ do
    children <- readTVar (reteNodeChildren node)
    unless (null children) $ do
      -- Matching wmes are taken from the α memory indexes
      wmes <- matchingAmemWmes (joinTests variant) tok amem
      -- Iterate with all wmes over all child nodes and left-activate
      forM_ wmes $ \wme -> do
        let wme' = Just wme
        forM_ children $ \child ->
          leftActivate env tok wme' child

  return ()
{-# INLINABLE leftActivateJoinNode #-}

rightActivateJoinNode :: Env -> WME -> ReteNode -> ReteNodeVariant -> STM ()
rightActivateJoinNode env wme node variant = do
  let amem        = nodeAmem variant
      Just parent = reteNodeParent node
      parentVariant = reteNodeVariant parent
      -- safely above, JoinNodes always have parents

  parentToksCount <- readTVar (nodeTokensCount parentVariant)

  -- When node.amem just became non-empty (equivalently testing for
  -- the left-unlinked flag)
  leftUnlinked' <- readTVar (leftUnlinked variant)
  when leftUnlinked' $ do
    -- Relink this node to parent (β memory)
    modifyTVar' (reteNodeChildren parent) (node:)
    writeTVar   (leftUnlinked variant) False

    -- When the parent β memory is empty
    when (parentToksCount == 0) $ do
      -- Right-unlink this node
      modifyTVar' (amemSuccessors amem) (filter (/= node))
      writeTVar (rightUnlinked variant) True

  when (parentToksCount /= 0) $ do
    children <- readTVar (reteNodeChildren node)
    unless (null children) $ do
      parentToks <- readTVar (nodeTokens parentVariant)
      unless (Set.null parentToks) $ do
        let tests = joinTests variant
            wme'  = Just wme
        forM_ (Set.toList parentToks) $ \tok ->
          when (performJoinTests tests tok wme) $
            forM_ children $ \child ->
              leftActivate env tok wme' child
{-# INLINABLE rightActivateJoinNode #-}

-- NEGATIVE NODES

leftActivateNegativeNode ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivateNegativeNode env tok wme node variant = do
  rightUnlinked' <- readTVar (rightUnlinked variant)
  when rightUnlinked' $ do
    -- The rightUnlinked status must be checked here because a
    -- negative node is not right unlinked on creation.
    toksCount <- readTVar (nodeTokensCount variant)
    when (toksCount == 0) $ do
      relinkToAlphaMemory node variant
      writeTVar (rightUnlinked variant) False

  -- Build a new token and store it just like a β memory would
  newTok <- makeAndInsertToken env tok wme node variant

  let amem = nodeAmem variant
  wmesCount <- readTVar (amemWmesCount amem)

  -- Compute the join results (using α memory indexes)
  when (wmesCount /= 0) $ do
    wmes <- matchingAmemWmes (joinTests variant) newTok amem
    forM_ wmes $ \wme' -> do
      let jr = NegativeJoinResult newTok wme'
      modifyTVar' (tokNegJoinResults newTok) (Set.insert jr)
      modifyTVar' (wmeNegJoinResults wme')   (Set.insert jr)
      -- In the original Doorenbos pseudo-code there was a bug - wme
      -- was used instead of wme' in the 3 lines above.

  -- If join results are empty, then inform children
  jresults <- readTVar (tokNegJoinResults newTok)
  unless (Set.null jresults) $ do
    children <- readTVar (reteNodeChildren node)
    mapM_ (leftActivate env newTok Nothing) children
{-# INLINABLE leftActivateNegativeNode #-}

rightActivateNegativeNode :: Env -> WME -> ReteNode -> ReteNodeVariant -> STM ()
rightActivateNegativeNode _ wme _ variant = do
  toks <- readTVar (nodeTokens variant)
  unless (Set.null toks) $ do
    let tests = joinTests variant
    forM_ (Set.toList toks) $ \tok ->
      when (performJoinTests tests tok wme) $ do
        joinResults <- readTVar (tokNegJoinResults tok)
        when (Set.null joinResults) (deleteDescendentsOfToken tok)

        let jr = NegativeJoinResult tok wme
        -- insert jr into tok.(neg)join-results
        modifyTVar' (tokNegJoinResults tok) (Set.insert jr)
        -- insert jr into wme.neg-join-results
        modifyTVar' (wmeNegJoinResults wme) (Set.insert jr)
{-# INLINABLE rightActivateNegativeNode #-}

-- NCC NODES

leftActivateNCCNode ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivateNCCNode _ _ _ _ _ = undefined -- TODO
{-# INLINABLE leftActivateNCCNode #-}

-- NCC PARNTER NODES

leftActivateNCCPartner ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivateNCCPartner _ _ _ _ _ = undefined -- TODO
{-# INLINABLE leftActivateNCCPartner #-}

-- P(RODUCTION) NODES

leftActivatePNode ::
  Env -> Token -> Maybe WME -> ReteNode -> ReteNodeVariant -> STM ()
leftActivatePNode _ _ _ _ _ = undefined
{-# INLINABLE leftActivatePNode #-}

-- LINKING/UNLINKING

relinkToAlphaMemory :: ReteNode -> ReteNodeVariant -> STM ()
relinkToAlphaMemory node variant = do
  ancestorLookup <- relinkAncestor variant
  case ancestorLookup of
    Just ancestor ->
      -- insert node into node.amem.successors immediately before
      -- ancestor
      modifyTVar' (amemSuccessors (nodeAmem variant))
        (node `insertBefore` ancestor)

    -- insert node at the tail of node.amem.successors
    Nothing -> modifyTVar' (amemSuccessors (nodeAmem variant)) (++ [node])
{-#  INLINABLE relinkToAlphaMemory #-}

-- | The goal of this loop is to find the nearest right linked
-- ancestor with the same α memory.
relinkAncestor :: ReteNodeVariant -> STM (Maybe ReteNode)
relinkAncestor variant =
  case nearestAncestorWithSameAmem variant of
    Nothing -> return Nothing
    Just ancestor -> do
      let ancestorVariant = reteNodeVariant ancestor
      rightUnlinked' <- readTVar (rightUnlinked ancestorVariant)
      if rightUnlinked'
        then relinkAncestor ancestorVariant
        else return (Just ancestor)
{-#  INLINABLE relinkAncestor #-}

-- | Deletes the descendents of the passed token.
deleteDescendentsOfToken :: Token -> STM ()
deleteDescendentsOfToken _ = undefined

