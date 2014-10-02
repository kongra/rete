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
-- Portability : requires stm
--
------------------------------------------------------------------------
module AI.Rete.Data where

import Control.Concurrent.STM
import Data.Hashable (Hashable, hashWithSalt)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

type TList a = TVar [a]
type TSet  a = TVar (Set.HashSet a)

-- | Identifier type. From now on we treat negative identifiers as
-- special ones, and the non-negative as auto-generated.
type ID = Int

-- | Symbols (including Variables)
data Symbol = Symbol   {-# UNPACK #-} !ID !String
            | Variable {-# UNPACK #-} !ID !String

-- | The registry of Symbols
type SymbolsRegistry = Map.HashMap String Symbol

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
  , envSymbolsRegistry :: {-# UNPACK #-} !(TVar SymbolsRegistry)

    -- | The Working Memory consists of a registry of all Wmes
    -- (indexed by WmeKey) and 3 Wme indexes by Wme Field value.
  , envWmesRegistry :: {-# UNPACK #-} !(TVar WmesRegistry)
  , envWmesByObj    :: {-# UNPACK #-} !(TVar WmesIndex)
  , envWmesByAttr   :: {-# UNPACK #-} !(TVar WmesIndex)
  , envWmesByVal    :: {-# UNPACK #-} !(TVar WmesIndex)

    -- | The registry of known α memories
  , envAmems :: {-# UNPACK #-} !(TVar AmemsRegistry)
  }

-- | Working Memory Element (Wme)
data Wme =
  Wme
  {
    -- | An internal identifier of this wme
    wmeId :: {-# UNPACK #-} !ID

    -- | fields
  , wmeObj  :: !Symbol
  , wmeAttr :: !Symbol
  , wmeVal  :: !Symbol

    -- | α-memories this Wme belongs to (8 at most)
  , wmeAmems :: {-# UNPACK #-} !(TList Amem)

    -- | Tokens with tokenWme = this wme.
  , wmeTokens :: {-# UNPACK #-} !(TSet Token)

    -- | Negative join results in which this wme participates
  , wmeNegJoinResults :: {-# UNPACK #-} !(TSet NegativeJoinResult)
  }

instance Show Wme where
  show Wme {wmeObj=obj, wmeAttr=attr, wmeVal=val} =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"

instance Eq Wme where
  wme1 == wme2 = wmeId wme1 == wmeId wme2

instance Hashable Wme where
  hashWithSalt salt wme = salt `hashWithSalt` wmeId wme

-- | Token. We introduce the same structure for standard tokens and
-- for the Dummy Top Token.
data Token =
  Token
  {
    -- | An internal identifier of the token
    tokId :: {-# UNPACK #-} !ID

    -- | Points to a higher token
  , tokParent :: !Token

    -- | i-th Wme, Nothing for some tokens
  , tokWme :: !(Maybe Wme)

    -- | The node the token is in
  , tokNode :: !Node

    -- | The Tokens with parent = this
  , tokChildren :: {-# UNPACK #-} !(TSet Token)

    -- | Used only for Tokens in negative nodes
  , tokNegJoinResults :: {-# UNPACK #-} !(TSet NegativeJoinResult)

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: {-# UNPACK #-} !(TSet Token)

    -- | On Tokens in NCC partners: tokens in whose local memory this
    -- result resides
  , tokOwner :: {-# UNPACK #-} !(TVar (Maybe Token))
  }
  |
  DummyTopToken
  {
    -- | The node the token is in - DummyTopNode
    tokNode :: !Node

    -- | The Tokens with parent = this
  , tokChildren :: {-# UNPACK #-} !(TSet Token)
  }

instance Eq Token where
  Token {tokId = id1} == Token {tokId = id2} = id1 == id2
  DummyTopToken {}    == DummyTopToken {}    = True
  _ == _ = False

instance Hashable Token where
  hashWithSalt salt (Token {tokId = id'}) = salt `hashWithSalt` id'
  hashWithSalt salt (DummyTopToken {})    = salt `hashWithSalt` ((-1) :: ID)

-- | Field
data Field = Obj | Attr | Val deriving (Show)

-- | α Memory Index
type WmesIndex = (Map.HashMap Symbol (Set.HashSet Wme))

-- | α Memory
data Amem =
  Amem
  { -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: {-# UNPACK #-} !(TList Node)

    -- | The number of join or negative node using this Amem
  , amemReferenceCount :: {-# UNPACK #-} !(TVar Int)

    -- | The wmes in this α memory (unindexed)
  , amemWmes :: {-# UNPACK #-} !(TSet Wme)

    -- | Wmes are indexed by their Field value.
  , amemWmesByObj  :: {-# UNPACK #-} !(TVar WmesIndex)
  , amemWmesByAttr :: {-# UNPACK #-} !(TVar WmesIndex)
  , amemWmesByVal  :: {-# UNPACK #-} !(TVar WmesIndex)

    -- | Keys to identify the α memory in the α memories registry
  , amemObj  :: !Symbol
  , amemAttr :: !Symbol
  , amemVal  :: !Symbol
  }

-- | Node with Variants
data Node =
  Node
  {
    nodeId :: {-# UNPACK #-} !ID -- ^ an internal ID

  , nodeParent :: !Node

    -- | children must be a list, because the ordering matters,
    -- e.g. for NCC networks
  , nodeChildren :: {-# UNPACK #-} !(TList Node)

  , nodeVariant :: !NodeVariant
  }
  |
  DummyTopNode
  {
    -- | children must be a list, because the ordering matters,
    -- e.g. for NCC networks
    nodeChildren :: {-# UNPACK #-} !(TList Node)

  , nodeVariant :: !NodeVariant  -- DTN
  }

instance Eq Node where
  Node {nodeId = id1} == Node {nodeId = id2} = id1 == id2
  DummyTopNode {}     == DummyTopNode {}     = True
  _ == _ = False

data NodeVariant =
  DTN
  {
    -- | Like in a β memory, but contains only a single DummyTopToken.
    nodeTokens :: {-# UNPACK #-} !(TSet Token)

    -- | Like in a β memory (see below)
  , bmemAllChildren :: {-# UNPACK #-} !(TSet Node)
  }
  |
  Bmem
  {
    nodeTokens :: {-# UNPACK #-} !(TSet Token)

    -- | With left unlinking, we need this list to be able to find and
    -- share also the currently left-unlinked nodes.
  , bmemAllChildren :: {-# UNPACK #-} !(TSet Node)
  }
  |
  JoinNode
  {
    -- | Points to the α memory this node is attached to.
    nodeAmem :: !Amem
  , nearestAncestorWithSameAmem :: !(Maybe Node)

  , joinTests :: ![JoinTest]

  , leftUnlinked  :: {-# UNPACK #-} !(TVar Bool)
  , rightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NegativeNode
  {
    nodeTokens :: {-# UNPACK #-} !(TSet Token)

    -- | The α memory this node is attached to (like for JoinNode)
  , nodeAmem :: !Amem

  , joinTests :: ![JoinTest]
  , nearestAncestorWithSameAmem :: !(Maybe Node)

    -- | There is no left unlinking for negative nodes
  , rightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NCCNode
  {
    nodeTokens :: {-# UNPACK #-} !(TSet Token)
  , nccPartner :: !Node  -- ^ with NCCPartner variant
  }
  |
  NCCPartner
  {
    -- | A corresponding NCC node, must be a TVar because of the
    -- circular dependency. See nccPartner in NCCNode variant.
    nccPartnerNccNode :: {-# UNPACK #-} !(TVar Node)

  , nccPartnerNumberOfConjucts :: {-# UNPACK #-} !Int

    -- | Results for the match the NCC node hasn't heard about
  , nccPartnerNewResultBuffer :: {-# UNPACK #-} !(TSet Token)
  }
  |
  PNode
  {
    nodeTokens :: {-# UNPACK #-} !(TSet Token)

  , pnodeName :: !String  -- ^ Name of the production

    -- | The action to fire on activation
  , pnodeAction :: !Action

    -- | The optional action to fire on token deletion
  , pnodeRevokeAction :: !(Maybe Action)

    -- | Variable bindings for the production
  , pnodeVariableBindings :: !VariableBindings
  }

-- The Dummy Top Node is a β memory with no parent and a single dummy
-- top token.

-- | JoinTest
data JoinTest =
  JoinTest
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
  , negativeJoinResultWme   :: !Wme
  }
  deriving (Eq)

instance Hashable NegativeJoinResult where
  hashWithSalt salt (NegativeJoinResult owner wme) =
    salt `hashWithSalt` owner `hashWithSalt` wme

-- | TokenLocation describes the binding for a variable within a token.
data TokenLocation = TokenLocation
                     !Field               -- ^ the field w in Wme
                     {-# UNPACK #-} !Int  -- ^ distance within the token

-- | A map of variable bindings for productions
type VariableBindings = Map.HashMap Symbol TokenLocation

-- | Actions
type Action = Env        -- ^ Environment
              -> Node    -- ^ the one having PNode variant
              -> Token   -- ^ The matching token
              -> STM ()

-- | The Working Memory key
data WmeKey = WmeKey
              !Symbol  -- ^ obj
              !Symbol  -- ^ attr
              !Symbol  -- ^ val
            deriving Eq

instance Hashable WmeKey where
  hashWithSalt salt (WmeKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

-- | The Working Memory is actually a Wme registry within the Env
type WmesRegistry = (Map.HashMap WmeKey Wme)

-- | The registry of known α memories within the Env
type AmemsRegistry = (Map.HashMap WmeKey Amem)
