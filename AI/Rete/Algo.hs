{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
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
-- This module contains mechanisms of managing facts and propagating
-- related within the Rete network.
------------------------------------------------------------------------
module AI.Rete.Algo
    (
      -- * 'Env' related operations
      createEnv
    , genstr
    , genstrD
    , genid

      -- * 'Wme' operations
    , addWme
    , addWmeA
    , removeWme
    , removeWmeA

      -- * (Internal) utilities
    , vprop
    , rvprop
    , nullTSet
    , toListT
    , fieldValue
    , getId
    , tokWmes

      -- * (Internal) 'Symbol' stuff
    , wildcardSymbol
    , internSymbol
    , sToSymbol

      -- * (Internal) activation procedures
    , leftActivate
    , rightActivate

      -- * (Internal) UL
    , rightUnlink
    , isRightUnlinked

      -- * (Internal) 'NodeVariant' predicates
    , isJoinNode
    , isNegativeNode
    , isNccNode
    , isNccPartner

      -- * (Internal) data manipulation
    , wmesIndexInsert
    , deleteTokenAndDescendents
    )
    where

import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Exception (Exception)
import           Control.Monad (when, unless, liftM, liftM2, forM_)
import           Data.Foldable (Foldable, toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as Seq
import           Data.Typeable
import           Kask.Control.Monad (whenM, unlessM, mapMM_, forMM_, toListM)
import           Kask.Data.Sequence (insertBeforeFirstOccurence,
                                     removeFirstOccurence)
import           Safe (headMay)

-- MISC. UTILS

-- | A monadic (in STM monad) version of Set.null.
nullTSet :: TSet a -> STM Bool
nullTSet = liftM Set.null . readTVar
{-# INLINE nullTSet #-}

-- | A monadic (in STM monad) version of Data.Foldable.toList.
toListT :: Foldable f => TVar (f a) -> STM [a] -- TSeq a -> STM [a]
toListT = toListM . readTVar
{-# INLINE toListT #-}

-- VARIANTS

-- | Accesses the variant property of the node.
vprop :: (NodeVariant -> a) -> Node -> a
vprop accessor = accessor . nodeVariant
{-# INLINE vprop #-}

-- | Accesses the transactional variant property of the node and reads
-- it'c content.
rvprop :: (NodeVariant -> TVar a) -> Node -> STM a
rvprop accessor = readTVar . vprop accessor
{-# INLINE rvprop #-}

-- Variant type recognition could be implemented using TemplateHaskell
-- and derive, but let's keep dependencies slim if possible. See:
-- http://stackoverflow.com/questions/6088935/
-- checking-for-a-particular-data-constructor

-- | Returns True iff the node is a JoinNode.
isJoinNode :: Node -> Bool
isJoinNode node = case nodeVariant node of
  (JoinNode {}) -> True
  _             -> False
{-# INLINE isJoinNode #-}

-- | Returns True iff the node is a NegativeNode.
isNegativeNode :: Node -> Bool
isNegativeNode node = case nodeVariant node of
  (NegativeNode {}) -> True
  _                 -> False
{-# INLINE isNegativeNode #-}

-- | Returns True iff the node is a Ncc node.
isNccNode :: Node -> Bool
isNccNode node = case nodeVariant node of
  (NccNode {}) -> True
  _            -> False
{-# INLINE isNccNode #-}

-- | Returns True iff the node is a Ncc partner.
isNccPartner :: Node -> Bool
isNccPartner node = case nodeVariant node of
  (NccPartner {}) -> True
  _               -> False
{-# INLINE isNccPartner #-}

-- | Represents the objects that have an accessible ID
class WithId a where getId :: a -> ID

instance WithId Symbol where
  getId (Symbol   id' _) = id'
  getId (Variable id' _) = id'

instance WithId Wme where getId Wme {wmeId = id'} = id'

instance WithId Token where
  getId Token         {tokId = id'} = id'
  getId DummyTopToken {}            = -1

instance WithId Node where
  getId Node         {nodeId = id'} = id'
  getId DummyTopNode {}             = -1

-- ENVIRONMENT, GENERATING IDS

-- | Creates a new Env
createEnv :: STM Env
createEnv = do
  id'     <- newTVar 0
  symbols <- newTVar Map.empty
  wmes    <- newTVar Map.empty
  objs    <- newTVar Map.empty
  attrs   <- newTVar Map.empty
  vals    <- newTVar Map.empty
  amems   <- newTVar Map.empty
  prods   <- newTVar Set.empty

  -- Initialize dummies
  dummyNodeChildren    <- newTVar Seq.empty
  dummyNodeTokens      <- newTVar Set.empty
  dummyNodeAllChildren <- newTVar Set.empty
  dummyTokChildren     <- newTVar Set.empty

  let dummyVariant = DTN           dummyNodeTokens   dummyNodeAllChildren
      dummyNode    = DummyTopNode  dummyNodeChildren dummyVariant
      dummyTok     = DummyTopToken dummyNode         dummyTokChildren

  -- Make the dummy token the only member of dummyNode.tokens
  modifyTVar' dummyNodeTokens (Set.insert dummyTok)

  return Env { envId              = id'
             , envSymbolsRegistry = symbols
             , envWmesRegistry    = wmes
             , envWmesByObj       = objs
             , envWmesByAttr      = attrs
             , envWmesByVal       = vals
             , envAmems           = amems
             , envDummyTopNode    = dummyNode
             , envDummyTopToken   = dummyTok
             , envProductions     = prods }
{-# INLINABLE createEnv #-}

data IDOverflow = IDOverflow deriving (Show, Typeable)
instance Exception IDOverflow

-- | Generates a new ID
genid :: Env -> STM ID
genid Env {envId = eid} = do
  recent <- readTVar eid

  -- Hopefully not in a reasonable time, but ...
  when (recent == maxBound) (throwSTM IDOverflow)

  let new = recent + 1
  writeTVar eid $! new
  return new
{-# INLINABLE genid #-}

-- | Generates a new string with a unique name. The result is prefix#
-- where # is some unique number.
genstr :: String -> Env -> STM String
genstr prefix env = do
  id' <- genid env
  return (prefix ++ show id')
{-# INLINE genstr #-}

-- | The default unique string generator that uses "G__" prefix
genstrD :: Env -> STM String
genstrD = genstr "G__"
{-# INLINE genstrD #-}

type WmesIndexOperator = Symbol -> Wme -> WmesIndex -> WmesIndex

-- | A component mechanism working when adding/removing wmes to/from
-- the indexes in the environment.
updateEnvIndexes :: WmesIndexOperator -> Env -> Wme -> STM ()
updateEnvIndexes f env wme = do
  let byObjIndex  = envWmesByObj  env
      byAttrIndex = envWmesByAttr env
      byValIndex  = envWmesByVal  env

  modifyTVar' byObjIndex  (f (wmeObj wme)   wme)
  modifyTVar' byObjIndex  (f wildcardSymbol wme)

  modifyTVar' byAttrIndex (f (wmeAttr wme)  wme)
  modifyTVar' byAttrIndex (f wildcardSymbol wme)

  modifyTVar' byValIndex  (f (wmeVal wme)   wme)
  modifyTVar' byValIndex  (f wildcardSymbol wme)
{-# INLINE updateEnvIndexes #-}

-- SYMBOLS, INTERNING

emptySymbol :: Symbol
emptySymbol = Symbol (-1) ""

emptyVariable :: Symbol
emptyVariable = Variable (-2) "?"

wildcardSymbol :: Symbol
wildcardSymbol = Symbol (-3) "* (wildcard symbol)"

-- | If there is an interned symbol of the given name, it is returned,
-- Nothing otherwise. The routine is intended to be used as an
-- interning-status predicate.
internedSymbol ::  String -> SymbolsRegistry -> Maybe Symbol
internedSymbol ""       _ = Just emptySymbol

internedSymbol name@[x] registry
  | x == '?'  = Just emptyVariable
  | otherwise = Map.lookup name registry

internedSymbol name@(_:_) registry = Map.lookup name registry
{-# INLINE internedSymbol #-}

-- | Interns a symbol represented by the string argument.
internSymbol :: Env -> String -> STM Symbol
internSymbol _ "" = return emptySymbol

internSymbol env name@[x]
  | x == '?'  = return emptyVariable
  | otherwise = internSymbolImpl env name Symbol

internSymbol env name@(x:_)
  | x == '?'  = internSymbolImpl env name Variable
  | otherwise = internSymbolImpl env name Symbol
{-# INLINABLE internSymbol #-}

type SymbolConstructor = ID -> String -> Symbol

internSymbolImpl :: Env -> String -> SymbolConstructor -> STM Symbol
internSymbolImpl env name constr = do
  registry <- readTVar (envSymbolsRegistry env)
  case Map.lookup name registry of
    Just s  -> return s
    Nothing -> do
      id' <- genid env
      let s = constr id' name
      modifyTVar' (envSymbolsRegistry env) (Map.insert name s)
      return s
{-# INLINABLE internSymbolImpl #-}

-- | Converts the S object into a Symbol.
sToSymbol :: Env -> S -> STM Symbol
sToSymbol _   (Sym s) = return s
sToSymbol env (S   s) = internSymbol env s
{-# INLINE sToSymbol #-}

-- | Converts the S object into a Symbol, but only if the symbol was
-- interned into the registry of symbols. If not - returns Nothing.
sToMaybeSymbol :: SymbolsRegistry -> S -> Maybe Symbol
sToMaybeSymbol _        (Sym s) = Just s
sToMaybeSymbol registry (S   s) = internedSymbol s registry
{-# inline sToMaybeSymbol #-}

-- WMES INDEXES MANIPULATION

-- | Creates an updated version of the wme index by putting a new
-- wme under the key s.
wmesIndexInsert :: WmesIndexOperator
wmesIndexInsert s wme index = Map.insert s newSet index
  where oldSet = Map.lookupDefault Set.empty s index
        newSet = Set.insert wme oldSet
{-# INLINABLE wmesIndexInsert #-}

-- | Removes the passed wme (possibly) stored under the key s from the
-- index.
wmesIndexDelete :: WmesIndexOperator
wmesIndexDelete s wme index =
  case Map.lookup s index of
    Nothing     -> index
    Just oldSet -> Map.insert s newSet index
      where newSet = Set.delete wme oldSet
{-# INLINABLE wmesIndexDelete #-}

-- α MEMORY, ACTIVATION

-- | Activates the α memory
activateAmem :: Env -> Amem -> Wme -> STM ()
activateAmem env amem wme = do
  -- put amem to wme registry of Amems
  modifyTVar' (wmeAmems wme) (amem:)

  -- put wme into the amem
  modifyTVar' (amemWmes amem) (Set.insert wme)

  -- put wme into amem indexes
  modifyTVar' (amemWmesByObj  amem) (wmesIndexInsert (wmeObj  wme) wme)
  modifyTVar' (amemWmesByAttr amem) (wmesIndexInsert (wmeAttr wme) wme)
  modifyTVar' (amemWmesByVal  amem) (wmesIndexInsert (wmeVal  wme) wme)

  -- right-activate all successors (children)
  mapMM_ (rightActivate env wme) (toListT (amemSuccessors amem))
{-# INLINABLE activateAmem #-}

-- | Propagates the wme into the corresponding α memories and further
-- down the network
feedAmems :: Env -> Wme -> Symbol -> Symbol -> Symbol -> STM ()
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

-- | Propagates the wme into the α memory and further down the
-- network.
feedAmem :: Env -> Wme -> Symbol -> Symbol -> Symbol -> STM ()
feedAmem env wme obj attr val = do
  amems <- readTVar (envAmems env)
  case Map.lookup (WmeKey obj attr val) amems of
    -- Activate amem propagating to successors.
    Just amem -> activateAmem env amem wme
    Nothing   -> return ()
{-# INLINABLE feedAmem #-}

-- WMES, ADDING

class AddWme a where
  -- | Adds the fact represented by the strings to the working memory
  -- and propagates the change downwards the Rete network. Returns the
  -- corresponding Wme. If the fact was already present, nothing happens.
  addWme :: Env -> a -> a -> a -> STM (Maybe Wme)

instance AddWme String where
  addWme env obj attr val = do
    obj'  <- internSymbol env obj
    attr' <- internSymbol env attr
    val'  <- internSymbol env val
    addWme env obj' attr' val'

instance AddWme S where
  addWme env obj attr val = do
    obj'  <- sToSymbol env obj
    attr' <- sToSymbol env attr
    val'  <- sToSymbol env val
    addWme env obj' attr' val'

instance AddWme Symbol where
  addWme env obj attr val = do
    let wmesRegistry = envWmesRegistry env
    wr <- readTVar wmesRegistry
    let k = WmeKey obj attr val

    if Map.member k wr
      then return Nothing  -- Already present, do nothing.
      else do
        wme <- createWme env obj attr val
        -- Add to the global wmes registry under key k.
        writeTVar wmesRegistry $! Map.insert k wme wr

        -- Add to indices, including the wildcard symbol key.
        updateEnvIndexes wmesIndexInsert env wme

        -- Propagate into the α memories.
        feedAmems env wme obj attr val
        return (Just wme)

-- | Works like addWme inside an action (of a production).
addWmeA :: AddWme a => Actx -> a -> a -> a -> STM (Maybe Wme)
addWmeA actx = addWme (actxEnv actx)
{-# INLINE addWmeA #-}

-- | Creates an empty Wme
createWme :: Env -> Symbol -> Symbol -> Symbol -> STM Wme
createWme env obj attr val = do
  id'       <- genid   env
  amems     <- newTVar []
  toks      <- newTVar Set.empty
  njResults <- newTVar Set.empty

  return Wme { wmeId             = id'
             , wmeObj            = obj
             , wmeAttr           = attr
             , wmeVal            = val
             , wmeAmems          = amems
             , wmeTokens         = toks
             , wmeNegJoinResults = njResults }
{-# INLINE createWme #-}

-- TOKENS CREATION AND UTILS

makeToken :: Env -> Token -> Maybe Wme -> Node -> STM Token
makeToken env parentTok wme node = do
  id'        <- genid   env
  children   <- newTVar Set.empty
  njResults  <- newTVar Set.empty
  nccResults <- newTVar Set.empty
  owner      <- newTVar Nothing

  let tok = Token { tokId             = id'
                  , tokParent         = parentTok
                  , tokWme            = wme
                  , tokNode           = node
                  , tokChildren       = children
                  , tokNegJoinResults = njResults
                  , tokNccResults     = nccResults
                  , tokOwner          = owner}

  -- Add tok to parentTok.children (for tree-based removal)
  modifyTVar' (tokChildren parentTok) (Set.insert tok)

  when (isJust wme)
    -- Add tok to wme.tokens (for tree-based-removal)
    (modifyTVar' (wmeTokens (fromJust wme)) (Set.insert tok))

  return tok
{-# INLINABLE makeToken #-}

-- | Creates a token and adds it to the node.
makeAndInsertToken :: Env -> Token -> Maybe Wme -> Node -> STM Token
makeAndInsertToken env tok wme node = do
  newTok <- makeToken env tok wme node
  modifyTVar' (vprop nodeTokens node) (Set.insert newTok)
  return newTok
{-# INLINABLE makeAndInsertToken #-}

-- | Returns a sequence of wmes within the token. Every wme wrapped in
-- a Maybe instance due to the fact that some tokens do not carry on
-- any wme.
tokWmes :: Token -> [Maybe Wme]
tokWmes = map tokWme . tokWithAncestors
{-# INLINE tokWmes #-}

-- | Returns the parent of the token. Stays aware of the Dummy Top Token.
safeTokParent :: Token -> Maybe Token
safeTokParent Token {tokParent = parent} = Just parent
safeTokParent DummyTopToken {}           = Nothing
{-# INLINE safeTokParent #-}

safeMaybeTokParent :: Maybe Token -> Maybe Token
safeMaybeTokParent Nothing = Nothing
safeMaybeTokParent (Just tok) = safeTokParent tok
{-# INLINE safeMaybeTokParent #-}

-- | Returns a sequence of tokens starting with the argument and
-- following the parent(ship) relation.
tokWithAncestors :: Token -> [Token]
tokWithAncestors = map fromJust             -- safely strip-off Just
                   . init                   -- strip off Dummy Top Token
                   . takeWhile isJust       -- to avoid going into Nothings
                   . iterate safeMaybeTokParent
                   . Just
{-# INLINABLE tokWithAncestors #-}

-- | A safe (Maybe-aware) version of token wme accessor.
safeTokWme :: Maybe Token -> Maybe Wme
safeTokWme Nothing    = Nothing
safeTokWme (Just tok) = tokWme tok
{-# INLINE safeTokWme #-}

-- RIGHT ACTIVATION DISPATCH

-- | Right-activates the node with the passed wme
rightActivate :: Env -> Wme -> Node -> STM ()
rightActivate env wme node = case nodeVariant node of
  JoinNode     {} -> rightActivateJoinNode     env wme node
  NegativeNode {} -> rightActivateNegativeNode env wme node
  _               -> error "Illegal Node used to rightActivate"
{-# INLINABLE rightActivate #-}

-- LEFT ACTIVATION DISPATCH

-- | Left-activates the node
leftActivate :: Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivate env tok wme node = case nodeVariant node of
  Bmem         {} -> leftActivateBmem         env tok wme node
  JoinNode     {} -> leftActivateJoinNode     env tok wme node
  NegativeNode {} -> leftActivateNegativeNode env tok wme node
  NccNode      {} -> leftActivateNccNode      env tok wme node
  NccPartner   {} -> leftActivateNccPartner   env tok wme node
  PNode        {} -> leftActivatePNode        env tok wme node
  DTN          {} -> error "Can't happen, nobody activates Dummy Top Node."
{-# INLINABLE leftActivate #-}

-- β MEMORIES

leftActivateBmem ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivateBmem env tok wme node = do
  newTok <- makeAndInsertToken env tok wme node

  -- Left-activate children (solely JoinNodes, do not pass any wme)
  mapMM_ (leftActivate env newTok Nothing) (toListT (nodeChildren node))
{-# INLINABLE leftActivateBmem #-}

-- UNINDEXED JOIN TESTS

-- | Performs the join tests not using any kind of indexing. Useful
-- while right-activation, when the α memory passes a single wme, so
-- there is no use of the α memory indexing.
performJoinTests :: [JoinTest] -> Token -> Wme -> Bool
performJoinTests tests tok wme = all (passJoinTest tok wme) tests
{-# INLINE performJoinTests #-}

passJoinTest :: Token -> Wme -> JoinTest -> Bool
passJoinTest tok wme test = value1 == value2
  where
    value1    = fieldValue (joinTestField1 test) wme
    -- We feel safe to coerce to Just wme2 below because of the way
    -- the tests are constructed.
    Just wme2 = tokWmes tok !! joinTestDistance test
    value2    = fieldValue (joinTestField2 test) wme2
{-# INLINABLE passJoinTest #-}

-- | Reaches a value of the wme in the specified field.
fieldValue :: Field -> Wme -> Symbol
fieldValue Obj  = wmeObj
fieldValue Attr = wmeAttr
fieldValue Val  = wmeVal
{-# INLINE fieldValue #-}

-- INDEXED JOIN TESTS (USING α MEMORY INDEXES)

-- | Matches a token to wmes in an α memory using the α memory indexes.
matchingAmemWmes :: [JoinTest] -> Token -> Amem -> STM [Wme]
-- When no tests specified, we simply take all wmes from the α memory
matchingAmemWmes [] _ amem = toListT (amemWmes amem)
matchingAmemWmes tests tok amem = do
  -- When at least one test specified ...
  let wmes     = tokWmes tok
      (s:sets) = map (amemWmesForTest wmes amem) tests
  toListM (foldr (liftM2 Set.intersection) s sets)
{-# INLINABLE matchingAmemWmes #-}

-- | Uses proper indexes of the α memory to return a set of wmes
-- matching the token (represented as a sequence of Maybe Wme) wrt the
-- current test.
amemWmesForTest :: [Maybe Wme] -> Amem -> JoinTest -> STM (Set.HashSet Wme)
amemWmesForTest wmes amem test = do
  index <- amemIndexForField (joinTestField1 test) amem
  return (Map.lookupDefault Set.empty value index)
  where
    Just wme = wmes !! joinTestDistance test
    value    = fieldValue (joinTestField2 test) wme

    amemIndexForField Obj  = readTVar . amemWmesByObj
    amemIndexForField Attr = readTVar . amemWmesByAttr
    amemIndexForField Val  = readTVar . amemWmesByVal
{-# INLINABLE amemWmesForTest #-}

-- JOIN NODES

leftActivateJoinNode ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivateJoinNode env tok _ node = do
  let amem   = vprop nodeAmem node
      parent = nodeParent node

  isAmemEmpty <- nullTSet (amemWmes amem)

  -- When the parent just became non-empty (equivalently testing for
  -- the right-unlinked flag) ...
  whenM (isRightUnlinked node) $ do
    relinkToAlphaMemory node
    -- When amem is empty, left unlink this node
    when isAmemEmpty $ leftUnlink node parent

  unless isAmemEmpty $ do
    children <- readTVar (nodeChildren node)
    unless (Seq.null children) $ do
      -- Matching wmes are taken from the α memory indexes
      wmes <- matchingAmemWmes (vprop joinTests node) tok amem
      -- Iterate with all wmes over all child nodes and left-activate
      forM_ wmes $ \wme -> do
        let wme' = Just wme
        forM_ (toList children) $ \child ->
          leftActivate env tok wme' child
{-# INLINABLE leftActivateJoinNode #-}

rightActivateJoinNode :: Env -> Wme -> Node -> STM ()
rightActivateJoinNode env wme node = do
  let amem   = vprop nodeAmem node
      parent = nodeParent node

  isParentEmpty <- nullTSet (vprop nodeTokens parent)

  -- When node.amem just became non-empty (equivalently testing for
  -- the left-unlinked flag)
  whenM (isLeftUnlinked node) $ do
    -- Relink (left) this node to parent (β memory)
    relinkToParent node parent

    -- When the parent β memory is empty, right unlink
    when isParentEmpty $ rightUnlink node amem

  unless isParentEmpty $ do
    children <- readTVar (nodeChildren node)
    unless (Seq.null children) $ do
      parentToks <- rvprop nodeTokens parent
      unless (Set.null parentToks) $ do
        let tests = vprop joinTests node
            wme'  = Just wme
        forM_ (Set.toList parentToks) $ \tok ->
          when (performJoinTests tests tok wme) $
            forM_ (toList children) $ \child ->
              leftActivate env tok wme' child
{-# INLINABLE rightActivateJoinNode #-}

-- NEGATIVE NODES

leftActivateNegativeNode ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivateNegativeNode env tok wme node = do
  whenM (isRightUnlinked node) $
    -- The rightUnlinked status must be checked here because a
    -- negative node is not right unlinked on creation.
    whenM (nullTSet (vprop nodeTokens node)) $ relinkToAlphaMemory node

  -- Build a new token and store it just like a β memory would
  newTok <- makeAndInsertToken env tok wme node

  let amem = vprop nodeAmem node

  -- Compute the join results (using α memory indexes)
  unlessM (nullTSet (amemWmes amem)) $ do
    wmes <- matchingAmemWmes (vprop joinTests node) newTok amem
    forM_ wmes $ \wme' -> do
      let jr = NegativeJoinResult newTok wme'
      modifyTVar' (tokNegJoinResults newTok) (Set.insert jr)
      modifyTVar' (wmeNegJoinResults wme')   (Set.insert jr)
      -- In the original Doorenbos pseudo-code there was a bug - wme
      -- was used instead of wme' in the 3 lines above.

  -- If join results are empty, then inform children
  unlessM (nullTSet (tokNegJoinResults newTok)) $
    mapMM_ (leftActivate env newTok Nothing) (toListT (nodeChildren node))
{-# INLINABLE leftActivateNegativeNode #-}

rightActivateNegativeNode :: Env -> Wme -> Node -> STM ()
rightActivateNegativeNode env wme node  = do
  toks <- rvprop nodeTokens node
  unless (Set.null toks) $ do
    let tests = vprop joinTests node
    forM_ (Set.toList toks) $ \tok ->
      when (performJoinTests tests tok wme) $ do
        emptyjrs <- nullTSet (tokNegJoinResults tok)
        when emptyjrs (deleteDescendentsOfToken env tok)

        let jr = NegativeJoinResult tok wme
        -- insert jr into tok.(neg)join-results
        modifyTVar' (tokNegJoinResults tok) (Set.insert jr)
        -- insert jr into wme.neg-join-results
        modifyTVar' (wmeNegJoinResults wme) (Set.insert jr)
{-# INLINABLE rightActivateNegativeNode #-}

-- Ncc NODES

leftActivateNccNode ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivateNccNode env tok wme node = do
  -- Build and store a new token.
  newTok <- makeAndInsertToken env tok wme node
  let partner = vprop nccPartner node

  newResultBuffer <- rvprop nccPartnerNewResultBuffer partner

  -- It is true that during the algorithm the assignment takes place:
  -- new-token.ncc-results ← node.partner.new-result-buffer,
  -- so for clarity:
  let newTokNccResults = newResultBuffer

  -- Update new-token.ncc-results.
  writeTVar (tokNccResults newTok) $! newTokNccResults

  -- Clear the node.partner.new-result-buffer
  writeTVar (vprop nccPartnerNewResultBuffer partner) $! Set.empty

  -- Update result.owner ← new-token
  let jnewTok = Just newTok
  forM_ (Set.toList newTokNccResults) $ \result ->
    writeTVar (tokOwner result) $! jnewTok

  when (Set.null newTokNccResults) $
    -- no ncc results so inform children
    mapMM_ (leftActivate env newTok Nothing) (toListT (nodeChildren node))
{-# INLINABLE leftActivateNccNode #-}

-- Ncc PARNTER NODES

leftActivateNccPartner ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivateNccPartner env tok wme partner = do
  nccNode   <- rvprop nccPartnerNccNode partner
  newResult <- makeToken env tok wme partner

  -- Find appropriate owner token (into whose local memory we should
  -- put the new result).
  let (ownersTok, ownersWme) = findOwnersPair
                               (vprop nccPartnerNumberOfConjucts partner)
                               (Just tok)
                               wme
  owner <- findNccOwner (fromJust nccNode) ownersTok ownersWme
  case owner of
    Just owner' -> do
      -- Add newResult to owner's local memory and propagate further.
      modifyTVar' (tokNccResults owner') (Set.insert newResult)
      writeTVar   (tokOwner newResult) $! owner
      deleteDescendentsOfToken env owner'

    Nothing ->
      -- We didn't find an appropriate owner token already in the Ncc
      -- node's memory, so we just stuff the result in our temporary
      -- buffer.
      modifyTVar' (vprop nccPartnerNewResultBuffer partner)
        (Set.insert newResult)
{-# INLINABLE leftActivateNccPartner #-}

-- | Searches node.tokens for a tok such that tok.parent = ownersTok
-- and tok.wme = ownersWme.
findNccOwner :: Node -> Maybe Token -> Maybe Wme -> STM (Maybe Token)
findNccOwner node ownersTok ownersWme = do
  tokens <- rvprop nodeTokens node
  return $ headMay (filter matchingTok (Set.toList tokens))
  where
    matchingTok tok = isJust    ownersTok &&
                      tokParent tok == fromJust ownersTok &&
                      tokWme    tok == ownersWme
{-# INLINE findNccOwner #-}

-- | To find the appropriate owner token (into whose local memory we
-- should put the result tok), we must first figure out what pair
-- (owners-t, owners-w) would represent the owner. To do this we start
-- with the argument pair and walk up the right number of links to find
-- the pair that emerged from the join node for the condition preceding
-- the Ncc partner.
-- [From the original Doorenbos thesis]
findOwnersPair :: Int -> Maybe Token -> Maybe Wme -> (Maybe Token, Maybe Wme)
findOwnersPair numberOfConjucts ownersTok ownersWme =
  if numberOfConjucts == 0
    then (ownersTok, ownersWme)
    else findOwnersPair (numberOfConjucts - 1) ownersTok' ownersWme'
  where
    ownersWme' = safeTokWme         ownersTok
    ownersTok' = safeMaybeTokParent ownersTok
{-# INLINE findOwnersPair #-}

-- P(RODUCTION) NODES

leftActivatePNode ::
  Env -> Token -> Maybe Wme -> Node -> STM ()
leftActivatePNode env tok wme node  = do
  -- Create and insert a new token.
  newTok <- makeAndInsertToken env tok wme node

  -- Fire the proper action.
  let action = vprop pnodeAction node
  action (Actx env node newTok (tokWmes newTok))
{-# INLINABLE leftActivatePNode #-}

-- LINKING/UNLINKING

isRightUnlinked :: Node -> STM Bool
isRightUnlinked = readTVar . vprop rightUnlinked
{-# INLINE isRightUnlinked #-}

isLeftUnlinked :: Node -> STM Bool
isLeftUnlinked = readTVar . vprop leftUnlinked
{-# INLINE isLeftUnlinked #-}

-- | Performs the right-(re)linking to a proper α memory
relinkToAlphaMemory :: Node -> STM ()
relinkToAlphaMemory node = do
  ancestorLookup <- relinkAncestor node
  case ancestorLookup of
    Just ancestor ->
      -- insert node into node.amem.successors immediately before
      -- ancestor
      modifyTVar' (amemSuccessors (vprop nodeAmem node))
        (node `insertBeforeFirstOccurence` ancestor)

    -- insert node at the tail of node.amem.successors
    Nothing -> modifyTVar' (amemSuccessors (vprop nodeAmem node))
                           (Seq.|> node)

  writeTVar (vprop rightUnlinked node) False
{-#  INLINABLE relinkToAlphaMemory #-}

-- | The goal of this loop is to find the nearest right linked
-- ancestor with the same α memory.
relinkAncestor :: Node -> STM (Maybe Node)
relinkAncestor node =
  case vprop nearestAncestorWithSameAmem node of
    Nothing -> return Nothing
    Just ancestor -> do
      rightUnlinked' <- isRightUnlinked ancestor
      if rightUnlinked'
        then relinkAncestor ancestor
        else return (Just ancestor)
{-#  INLINABLE relinkAncestor #-}

-- | Right-unlinks from the α memory.
rightUnlink :: Node -> Amem -> STM ()
rightUnlink node amem = do
  modifyTVar' (amemSuccessors amem) (removeFirstOccurence node)
  writeTVar   (vprop rightUnlinked node) True
{-# INLINE rightUnlink #-}

-- | Performs the left-(re)linking to the parent node.
relinkToParent :: Node -> Node -> STM ()
relinkToParent node parent = do
  modifyTVar' (nodeChildren parent) (node Seq.<|)
  writeTVar   (vprop leftUnlinked node) False
{-# INLINE relinkToParent #-}

-- | Left-unlinks from the parent.
leftUnlink :: Node -> Node -> STM ()
leftUnlink node parent = do
  modifyTVar' (nodeChildren parent) (removeFirstOccurence node)
  writeTVar   (vprop leftUnlinked node) True
{-# INLINE leftUnlink #-}

-- REMOVING WMES

class RemoveWme a where
  -- | Removes the fact described by 3 symbols. Returns the removed wme
  -- or Nothing if the wme was not present in the working memory.
  removeWme :: Env -> a -> a -> a -> STM (Maybe Wme)

instance RemoveWme String where
  removeWme env obj attr val = do
    registry <- readTVar (envSymbolsRegistry env)
    let obj'  = internedSymbol obj  registry
        attr' = internedSymbol attr registry
        val'  = internedSymbol val  registry

    if isJust obj' && isJust attr' && isJust val'
      then removeWme env (fromJust obj') (fromJust attr') (fromJust val')

      -- At least 1 of the names didn't have a corresponding interned
      -- symbol, the wme can't exist.
      else return Nothing

instance RemoveWme S where
  removeWme env obj attr val = do
    registry <- readTVar (envSymbolsRegistry env)
    let obj'  = sToMaybeSymbol registry obj
        attr' = sToMaybeSymbol registry attr
        val'  = sToMaybeSymbol registry val

    if isJust obj' && isJust attr' && isJust val'
      then removeWme env (fromJust obj') (fromJust attr') (fromJust val')

      -- At least 1 of the names didn't have a corresponding interned
      -- symbol, the wme can't exist.
      else return Nothing

instance RemoveWme Symbol where
  removeWme env obj attr val = do
    let wmesRegistry = envWmesRegistry env
    wr <- readTVar wmesRegistry
    let k      = WmeKey obj attr val
        wmeLookup = Map.lookup k wr

    case wmeLookup of
      Nothing  -> return Nothing
      Just wme -> do
        -- Remove from Working Memory (Env registry and indices)
        writeTVar wmesRegistry $! Map.delete k wr
        updateEnvIndexes wmesIndexDelete env wme
        -- ... and propagate down the network.
        propagateWmeRemoval env wme
        return wmeLookup

-- | Works like removeWme inside an action (of a production).
removeWmeA :: RemoveWme a => Actx -> a -> a -> a -> STM (Maybe Wme)
removeWmeA actx = removeWme (actxEnv actx)
{-# INLINE removeWmeA #-}

-- | Propagates the wme removal down the Rete network. It is the
-- actual implementation of original remove-wme procedure from the
-- Doorenbos thesis.
propagateWmeRemoval :: Env -> Wme -> STM ()
propagateWmeRemoval env wme = do
  let obj  = wmeObj  wme
      attr = wmeAttr wme
      val  = wmeVal  wme

  -- For every amem this wme belongs to ...
  forMM_ (readTVar (wmeAmems wme)) $ \amem -> do
    -- ... remove wme from amem indexes
    modifyTVar' (amemWmesByObj  amem) (wmesIndexDelete obj  wme)
    modifyTVar' (amemWmesByAttr amem) (wmesIndexDelete attr wme)
    modifyTVar' (amemWmesByVal  amem) (wmesIndexDelete val  wme)
    wmes <- readTVar (amemWmes amem)
    let updatedWmes = Set.delete wme wmes
    writeTVar (amemWmes amem) $! updatedWmes

    when (Set.null updatedWmes) $
      -- ... left-unlink all successors of type JoinNode
      forMM_ (toListT (amemSuccessors amem)) $ \child ->
        when (isJoinNode child) $ leftUnlink child (nodeParent child)

  -- Delete all tokens wme is in. Remove every tokent from it's parent
  -- token but avoid removing from wme.
  mapMM_ (deleteTokenAndDescendents env True False)
    (toListT (wmeTokens wme))

  -- For every jr in wme.negative-join-results
  forMM_ (toListT (wmeNegJoinResults wme)) $ \jr -> do
    -- ... remove jr from jr.owner.negative-join-results
    let owner = negativeJoinResultOwner jr
    jresults <- readTVar (tokNegJoinResults owner)
    let updatedJresults = Set.delete jr jresults
    writeTVar (tokNegJoinResults owner) $! updatedJresults

    -- If jr.owner.negative-join-results is nil
    when (Set.null updatedJresults) $
      -- For each child in jr.owner.node.children
      mapMM_ (leftActivate env owner Nothing)
        ((toListT . nodeChildren . tokNode) owner)
{-# INLINABLE propagateWmeRemoval #-}

-- DELETING TOKENS

-- | Deletes the descendents of the passed token.
deleteDescendentsOfToken :: Env -> Token -> STM ()
deleteDescendentsOfToken env tok = do
  children <- readTVar (tokChildren tok)
  unless (Set.null children) $ do
    writeTVar (tokChildren tok) $! Set.empty
    -- Iteratively remove, skip removing from parent.
    mapM_ (deleteTokenAndDescendents env False True) (Set.toList children)
{-# INLINABLE deleteDescendentsOfToken #-}

-- | Deletes the token and it's descendents.
deleteTokenAndDescendents :: Env -> Bool -> Bool -> Token -> STM ()
deleteTokenAndDescendents env removeFromParent removeFromWme tok = do
  deleteDescendentsOfToken env tok

  let node = tokNode tok

  -- If tok.node is not Ncc partner ...
  unless (isNccPartner node) $
    -- ... remove tok from tok.node.items.
    modifyTVar' (vprop nodeTokens node) (Set.delete tok)

  when removeFromWme $ do
    let wme = tokWme tok
    when (isJust wme) $ -- If tok.wme /= null ...
      -- ... remove tok from tok.wme.tokens.
      modifyTVar' (wmeTokens (fromJust wme)) (Set.delete tok)

  when removeFromParent $
    -- Remove tok from tok.parent.children
     modifyTVar' (tokChildren (tokParent tok)) (Set.delete tok)

  -- Node variant-specific cleanup:
  case nodeVariant node of
    Bmem {} ->
      whenM (nullTSet (vprop nodeTokens node)) $
        forMM_ (toListT (nodeChildren node)) $ \child ->
          -- Beware, some children may not have amem, e.g. predicate
          -- nodes. If needed introduce a check here.
          rightUnlink child (vprop nodeAmem child)

    NegativeNode {} -> do
      whenM (nullTSet (vprop nodeTokens node)) $
        rightUnlink node (vprop nodeAmem node)

      -- For jr in tok.(neg)-join-results
      forMM_ (toListT (tokNegJoinResults tok)) $ \jr ->
        -- remove jr from jr.wme.negative-join-results
        modifyTVar' (wmeNegJoinResults (negativeJoinResultWme jr))
          (Set.delete jr)

    NccNode {} ->
      -- For result-tok in tok.ncc-results ...
      forMM_ (toListT (tokNccResults tok)) $ \resultTok -> do
          -- ... remove result-tok from result-tok.wme.tokens,
          modifyTVar' (wmeTokens (fromJust (tokWme resultTok)))
            (Set.delete resultTok)

          -- ... remove result-tok from result-tok.parent.children.
          modifyTVar' (tokChildren (tokParent resultTok))
            (Set.delete resultTok)

    NccPartner {} -> do
      (Just owner) <- readTVar (tokOwner tok)
      -- A token in Ncc partner always has owner.

      -- Remove tok from tok.owner.ncc-results
      nccResults <- readTVar (tokNccResults owner)
      let updatedNccResults = Set.delete tok nccResults
      writeTVar (tokNccResults owner) $! updatedNccResults

      -- If tok.owner.ncc-results is nil
      when (Set.null updatedNccResults) $ do
        nccNode <- rvprop nccPartnerNccNode node
        -- For child in tok.node.ncc-node.children -> leftActivate
        mapMM_ (leftActivate env owner Nothing)
          ((toListT . nodeChildren . fromJust ) nccNode)

    PNode {} ->
      -- For production nodes the only specific behavior is firing a
      -- proper action.
      case vprop pnodeRevokeAction node of
        Nothing     -> return ()
        Just action -> action (Actx env node tok (tokWmes tok))

    DTN      {} -> error "Deleting token(s) from Dummy Top Node is evil."
    JoinNode {} -> error "Can't happen."
{-# INLINABLE deleteTokenAndDescendents #-}
