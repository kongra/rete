{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Data
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-07-23
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
------------------------------------------------------------------------
module AI.Rete.Data where

import Control.Concurrent.STM
import Data.Hashable (Hashable, hashWithSalt)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- | Identifier type. From now on we treat negative identifiers as
-- special ones, and the non-negative as auto-generated.
type ID = Int

-- | Symbols (including Variables)
data Symbol = Symbol   {-# UNPACK #-} !ID !String
            | Variable {-# UNPACK #-} !ID !String

-- | The registry of Symbols
type SymbolRegistry = Map.HashMap String Symbol

instance Show Symbol where
  show (Symbol   _ s) = s
  show (Variable _ s) = s

instance Eq Symbol where
  (Symbol   id1 _) == (Symbol   id2 _) = id1 == id2
  (Variable id1 _) == (Variable id2 _) = id1 == id2
  _ == _ = False

instance Hashable Symbol where
  hashWithSalt salt (Symbol   id' _) = salt `hashWithSalt` id'
  hashWithSalt salt (Variable id' _) = salt `hashWithSalt` id'

-- | Environment
data Env =
  Env
  { -- | The state of the Env-wide ID generator
    envId :: {-# UNPACK #-} !(TVar ID)

    -- | The registry of (interned) Symbols
  , envSymbolsRegistry :: {-# UNPACK #-} !(TVar SymbolRegistry)
  }

-- | Working Memory Element (WME)
data WME =
  WME
  {
    -- | fields
    wmeObj  :: !Symbol
  , wmeAttr :: !Symbol
  , wmeVal  :: !Symbol

    -- | α-memories whis WME belongs to (8 at most)
  , wmeAmems :: {-# UNPACK #-} !(TVar [Amem])

    -- | Tokens with tokenWME = this WME.
  , wmeTokens :: {-# UNPACK #-} !(TVar (Set.HashSet Token))

    -- | Negative join results in which this WME participates
  , wmeNegJoinResults :: {-# UNPACK #-} !(TVar (Set.HashSet NegativeJoinResult))
  }

instance Show WME where
  show wme = "(" ++
             show obj  ++ "," ++
             show attr ++ "," ++
             show val  ++
             ")"
    where obj  = wmeObj wme
          attr = wmeAttr wme
          val  = wmeVal wme

instance Eq WME where
  wme1 == wme2 =
    wmeObj  wme1  == wmeObj  wme2 &&
    wmeAttr wme1  == wmeAttr wme2 &&
    wmeVal  wme1  == wmeVal  wme2

-- | Token. We introduce the same structure for standard tokens and
-- for the Dummy Top Token.
data Token =
  Token
  { -- | Points to a higher token, for items 1 .. i-1
    tokParent :: !(Maybe Token)

    -- | ith WME, Nothing for some tokens
  , tokWME :: !(Maybe WME)

    -- | The node the token is in
  , tokNode :: !ReteNode

    -- | The Tokens with parent = this
  , tokChildren :: {-# UNPACK #-} !(TVar (Set.HashSet Token))

    -- | Used only for Tokens in negative nodes
  , tokNegJoinResults :: {-# UNPACK #-} !(TVar (Set.HashSet NegativeJoinResult))

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: {-# UNPACK #-} !(TVar (Set.HashSet Token))

    -- | On Tokens in NCC partners: tokens in whose local memory this
    -- result resides
  , tokOwner :: !(Maybe Token)
  }

-- | Field
data Field = Obj | Attr | Val deriving (Show)

-- | α-Memory
type AmemIndex = (Map.HashMap Symbol (Set.HashSet WME))

data Amem =
  Amem
  { -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: {-# UNPACK #-} !(TVar [ReteNode])

    -- | The number of join or negative node using this Amem
  , amemReferenceCount :: {-# UNPACK #-} !(TVar Int)

    -- | Items are indexed by their Field value.
  , amemItemsByObj  :: {-# UNPACK #-} !(TVar AmemIndex)
  , amemItemsByAttr :: {-# UNPACK #-} !(TVar AmemIndex)
  , amemItemsByVal  :: {-# UNPACK #-} !(TVar AmemIndex)

    -- | The items count
  , amemItemsCount :: {-# UNPACK #-} !(TVar Int)

    -- TODO: wprowadzić klucze obj, attr, value przydatne przy
    -- usuwaniu pamięci -- patrz: rete.clj
  }

-- | ReteNode with Variants
data ReteNode =
  ReteNode
  {
    reteNodeParent   :: !(Maybe ReteNode)  -- ^ Nothing for DummyTopNode
    -- children must be a list, because the ordering matters,
    -- e.g. for NCC networks
  , reteNodeChildren :: {-# UNPACK #-} !(TVar [ReteNode])
  , reteNodeVariant  :: !ReteNodeVariant
  }

data ReteNodeVariant =
  BetaMemory
  {
    betaMemoryItems       :: {-# UNPACK #-} !(TVar (Set.HashSet Token))
  , betaMemoryItemsCount  :: {-# UNPACK #-} !(TVar Int)

    -- | With left unlinking, we need this list to be able to find and
    -- share also the currently left-unlinked nodes.
  , betaMemoryAllChildren :: {-# UNPACK #-} !(TVar (Set.HashSet ReteNode))
  }
  |
  JoinNode
  {
    -- | Points to the α memory this node is attached to.
    joinNodeAmem :: !Amem
    -- | nearest-ancestor-with-same-amem
  , joinNodeNearestAncestor :: !(Maybe ReteNode)

  , joinNodeTests :: ![JoinNodeTest]

  , joinNodeLeftUnlinked  :: {-# UNPACK #-} !(TVar Bool)
  , joinNodeRightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NegativeNode
  {
    negaviteNodeItems       :: {-# UNPACK #-} !(TVar (Set.HashSet Token))
  , negativeNodeItemsCount  :: {-# UNPACK #-} !(TVar Int)

    -- | The α memory this node is attached to (like for JoinNode)
  , negativeNodeAmem :: !Amem

  , negativeNodeTests :: ![JoinNodeTest]

    -- | nearest-ancestor-with-same-amem
  , negativeNodeNearestAncestor :: !(Maybe ReteNode)

    -- | There is no left unlinking for negative nodes
  , negativeNodeRightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NCCNode
  {
    nccNodeItems      :: {-# UNPACK #-} !(TVar (Set.HashSet Token))
  , nccNodeItemsCount :: {-# UNPACK #-} !(TVar Int)

  , nccPartner        :: !ReteNode  -- ^ with NCCPartnerNode variant
  }
  |
  NCCPartnerNode
  {
    -- | A corresponding NCC node, must be a TVar because of the
    -- circular dependency. See nccPartner in NCCNode variant.
    nccPartnerNccNode :: {-# UNPACK #-} !(TVar ReteNode)

  , nccPartnerNumberOfConjucts :: {-# UNPACK #-} !(TVar Int)

    -- | Results for the match the NCC node hasn't heard about
  , nccPartnerNewResultBuffer :: {-# UNPACK #-} !(TVar (Set.HashSet Token))
  }
  |
  PNode
  {
    pnodeItems :: {-# UNPACK #-} !(TVar (Set.HashSet Token))
  , pnodeName  :: !String  -- ^ Name of the production

    -- | The action to fire on activation
  , pnodeAction       :: !Action

    -- | The optional action to fire on token deletion
  , pnodeRevokeAction :: !Action

    -- | Variable bindings for the production
  , pnodeVariableBindings :: !VariableBindings
  }
  |
  DummyTopNode   -- ^ Acts as a special kind of β memory
  {
    -- | Like betaMemoryAllChildren
    dummyTopNodeAllChildren :: {-# UNPACK #-} !(TVar (Set.HashSet ReteNode))

    -- TODO: It's set of items consists only of a single dummy top token.
    --       It has no parent.
  }

-- | JoinNodeTest
data JoinNodeTest =
  JoinNodeTest
  {
    joinTestField1   :: !Field
  , joinTestField2   :: !Field
  , joinTestDistance :: {-# UNPACK #-} !Int
  }

-- | NegativeJoinResult
data NegativeJoinResult =
  NegativeJoinResult
  {
    negativeJoinResultOwner :: !Token
  , negativeJoinResultWme   :: !WME
  }

-- | TokenLocation describes the binding for a variable within a token.
data TokenLocation = TokenLocation
                     !Field  -- ^ the field w in WME
                     !Int    -- ^ distance within the token

-- | A map of variable bindings for productions
type VariableBindings = Map.HashMap Symbol TokenLocation

-- | Actions
type Action = Env          -- ^ Environment
              -> ReteNode  -- ^ with the PNode variant
              -> Token     -- ^ The matching token
              -> STM ()

-- SPOSTRZEŻENIA
-- * Prawdopodobnie prowostronne odłączanie ma większy wpływ na wydajność