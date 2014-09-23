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

type TList a   = TVar [a]
type TSet  a   = TVar (Set.HashSet a)

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

    -- | The Working Memory
  , envWorkingMemory :: {-# UNPACK #-} !(TVar WorkingMemory)

    -- | The registry of known α memories
  , envAmems :: {-# UNPACK #-} !(TVar AmemsRegistry)
  }

-- | Working Memory Element (WME)
data WME =
  WME
  {
    -- | An internal identifier of the wme
    wmeId :: {-# UNPACK #-} !ID

    -- | fields
  , wmeObj  :: !Symbol
  , wmeAttr :: !Symbol
  , wmeVal  :: !Symbol

    -- | α-memories whis WME belongs to (8 at most)
  , wmeAmems :: {-# UNPACK #-} !(TList Amem)

    -- | Tokens with tokenWME = this WME.
  , wmeTokens :: {-# UNPACK #-} !(TSet Token)

    -- | Negative join results in which this WME participates
  , wmeNegJoinResults :: {-# UNPACK #-} !(TSet NegativeJoinResult)
  }

instance Show WME where
  show WME {wmeObj=obj, wmeAttr=attr, wmeVal=val} =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"

instance Eq WME where
  wme1 == wme2 = wmeId wme1 == wmeId wme2

instance Hashable WME where
  hashWithSalt salt wme = salt `hashWithSalt` wmeId wme

-- | Token. We introduce the same structure for standard tokens and
-- for the Dummy Top Token.
data Token =
  Token
  {
    -- | An internal identifier of the token
    tokId :: {-# UNPACK #-} !ID

    -- | Points to a higher token
  , tokParent :: !(Maybe Token)

    -- | i-th WME, Nothing for some tokens
  , tokWme :: !(Maybe WME)

    -- | The node the token is in
  , tokNode :: !ReteNode

    -- | The Tokens with parent = this
  , tokChildren :: {-# UNPACK #-} !(TSet Token)

    -- | Used only for Tokens in negative nodes
  , tokNegJoinResults :: {-# UNPACK #-} !(TSet NegativeJoinResult)

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: {-# UNPACK #-} !(TSet Token)

    -- | On Tokens in NCC partners: tokens in whose local memory this
    -- result resides
  , tokOwner :: !(Maybe Token)
  }

instance Eq Token where
  tok1 == tok2 = tokId tok1 == tokId tok2

instance Hashable Token where
  hashWithSalt salt tok = salt `hashWithSalt` tokId tok

-- | Field
data Field = Obj | Attr | Val deriving (Show)

-- | α Memory Index
type AmemIndex = (Map.HashMap Symbol (Set.HashSet WME))

-- | α Memory
data Amem =
  Amem
  { -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: {-# UNPACK #-} !(TList ReteNode)

    -- | The number of join or negative node using this Amem
  , amemReferenceCount :: {-# UNPACK #-} !(TVar Int)

    -- | The wmes in this α memory (unindexed)
  , amemWmes :: {-# UNPACK #-} !(TSet WME)
    -- | The wmes count
  , amemWmesCount :: {-# UNPACK #-} !(TVar Int)

    -- | Wmes are indexed by their Field value.
  , amemWmesByObj  :: {-# UNPACK #-} !(TVar AmemIndex)
  , amemWmesByAttr :: {-# UNPACK #-} !(TVar AmemIndex)
  , amemWmesByVal  :: {-# UNPACK #-} !(TVar AmemIndex)

    -- | Keys to identify the α memory in the α memories registry
  , amemObj  :: !Symbol
  , amemAttr :: !Symbol
  , amemVal  :: !Symbol
  }

-- | ReteNode with Variants
data ReteNode =
  ReteNode
  {
    reteNodeId :: {-# UNPACK #-} !ID -- ^ an internal ID

  , reteNodeParent   :: !(Maybe ReteNode)  -- ^ Nothing for DummyTopNode
    -- children must be a list, because the ordering matters,
    -- e.g. for NCC networks
  , reteNodeChildren :: {-# UNPACK #-} !(TList ReteNode)
  , reteNodeVariant  :: !ReteNodeVariant
  }

instance Eq ReteNode where
  node1 == node2 = reteNodeId node1 == reteNodeId node2

data ReteNodeVariant =
  Bmem
  {
    nodeTokens      :: {-# UNPACK #-} !(TSet Token)
  , nodeTokensCount :: {-# UNPACK #-} !(TVar Int)

    -- | With left unlinking, we need this list to be able to find and
    -- share also the currently left-unlinked nodes.
  , bmemAllChildren :: {-# UNPACK #-} !(TSet ReteNode)
  }
  |
  JoinNode
  {
    -- | Points to the α memory this node is attached to.
    nodeAmem :: !Amem
  , nearestAncestorWithSameAmem :: !(Maybe ReteNode)

  , joinTests :: ![JoinTest]

  , leftUnlinked  :: {-# UNPACK #-} !(TVar Bool)
  , rightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NegativeNode
  {
    nodeTokens      :: {-# UNPACK #-} !(TSet Token)
  , nodeTokensCount :: {-# UNPACK #-} !(TVar Int)

    -- | The α memory this node is attached to (like for JoinNode)
  , nodeAmem :: !Amem

  , joinTests :: ![JoinTest]
  , nearestAncestorWithSameAmem :: !(Maybe ReteNode)

    -- | There is no left unlinking for negative nodes
  , rightUnlinked :: {-# UNPACK #-} !(TVar Bool)
  }
  |
  NCCNode
  {
    nodeTokens      :: {-# UNPACK #-} !(TSet Token)
  , nodeTokensCount :: {-# UNPACK #-} !(TVar Int)

  , nccPartner        :: !ReteNode  -- ^ with NCCPartner variant
  }
  |
  NCCPartner
  {
    -- | A corresponding NCC node, must be a TVar because of the
    -- circular dependency. See nccPartner in NCCNode variant.
    nccPartnerNccNode :: {-# UNPACK #-} !(TVar ReteNode)

  , nccPartnerNumberOfConjucts :: {-# UNPACK #-} !(TVar Int)

    -- | Results for the match the NCC node hasn't heard about
  , nccPartnerNewResultBuffer :: {-# UNPACK #-} !(TSet Token)
  }
  |
  PNode
  {
    nodeTokens      :: {-# UNPACK #-} !(TSet Token)
  , nodeTokensCount :: {-# UNPACK #-} !(TVar Int)
    
  , pnodeName  :: !String  -- ^ Name of the production

    -- | The action to fire on activation
  , pnodeAction :: !Action

    -- | The optional action to fire on token deletion
  , pnodeRevokeAction :: !Action

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
  , negativeJoinResultWme   :: !WME
  }
  deriving (Eq)

instance Hashable NegativeJoinResult where
  hashWithSalt salt (NegativeJoinResult owner wme) =
    salt `hashWithSalt` owner `hashWithSalt` wme

-- | TokenLocation describes the binding for a variable within a token.
data TokenLocation = TokenLocation
                     !Field               -- ^ the field w in WME
                     {-# UNPACK #-} !Int  -- ^ distance within the token

-- | A map of variable bindings for productions
type VariableBindings = Map.HashMap Symbol TokenLocation

-- | Actions
type Action = Env          -- ^ Environment
              -> ReteNode  -- ^ the one having PNode variant
              -> Token     -- ^ The matching token
              -> STM ()

-- | The Working Memory key
data WMEKey = WMEKey
              !Symbol  -- ^ obj
              !Symbol  -- ^ attr
              !Symbol  -- ^ val
            deriving Eq

instance Hashable WMEKey where
  hashWithSalt salt (WMEKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

-- | The Working Memory is actually a WME registry within the Env
type WorkingMemory = (Map.HashMap WMEKey WME)

-- | The registry of known α memories within the Env
type AmemsRegistry = (Map.HashMap WMEKey Amem)
