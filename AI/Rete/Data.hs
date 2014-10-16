{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
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
-- Contains the definitions of Rete data structures.
------------------------------------------------------------------------
module AI.Rete.Data where

import           Control.Concurrent.STM
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)

type TList a = TVar [a]
type TSet  a = TVar (Set.HashSet a)

-- | Identifier type. From now on we treat negative identifiers as
-- special ones, and the non-negative as auto-generated.
type ID = Int

-- | Symbols (including Variables)
data Symbol = Symbol   !ID !String
            | Variable !ID !String

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
    envId :: !(TVar ID)

    -- | The registry of (interned) Symbols
  , envSymbolsRegistry :: !(TVar SymbolsRegistry)

    -- | The Working Memory consists of a registry of all Wmes
    -- (indexed by WmeKey) and 3 Wme indexes by Wme Field value.
  , envWmesRegistry :: !(TVar WmesRegistry)
  , envWmesByObj    :: !(TVar WmesIndex)
  , envWmesByAttr   :: !(TVar WmesIndex)
  , envWmesByVal    :: !(TVar WmesIndex)

    -- | The registry of known α memories
  , envAmems :: !(TVar AmemsRegistry)
  }

-- | Working Memory Element (Wme)
data Wme =
  Wme
  {
    -- | An internal identifier of this wme
    wmeId :: !ID

    -- | fields
  , wmeObj  :: !Symbol
  , wmeAttr :: !Symbol
  , wmeVal  :: !Symbol

    -- | α-memories this Wme belongs to (8 at most)
  , wmeAmems :: !(TList Amem)

    -- | Tokens with tokenWme = this wme.
  , wmeTokens :: !(TSet Token)

    -- | Negative join results in which this wme participates
  , wmeNegJoinResults :: !(TSet NegativeJoinResult)
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
    tokId :: !ID

    -- | Points to a higher token
  , tokParent :: !Token

    -- | i-th Wme, Nothing for some tokens
  , tokWme :: !(Maybe Wme)

    -- | The node the token is in
  , tokNode :: !Node

    -- | The Tokens with parent = this
  , tokChildren :: !(TSet Token)

    -- | Used only for Tokens in negative nodes
  , tokNegJoinResults :: !(TSet NegativeJoinResult)

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: !(TSet Token)

    -- | On Tokens in NCC partners: tokens in whose local memory this
    -- result resides
  , tokOwner :: !(TVar (Maybe Token))
  }
  |
  DummyTopToken
  {
    -- | The node the token is in - DummyTopNode
    tokNode :: !Node

    -- | The Tokens with parent = this
  , tokChildren :: !(TSet Token)
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
    amemSuccessors :: !(TList Node)

    -- | The number of join or negative node using this Amem
  , amemReferenceCount :: !(TVar Int)

    -- | The wmes in this α memory (unindexed)
  , amemWmes :: !(TSet Wme)

    -- | Wmes are indexed by their Field value.
  , amemWmesByObj  :: !(TVar WmesIndex)
  , amemWmesByAttr :: !(TVar WmesIndex)
  , amemWmesByVal  :: !(TVar WmesIndex)

    -- | Keys to identify the α memory in the α memories registry
  , amemObj  :: !Symbol
  , amemAttr :: !Symbol
  , amemVal  :: !Symbol
  }

instance Eq Amem where
  Amem {amemObj = obj1, amemAttr = attr1, amemVal = val1} ==
    Amem {amemObj = obj2, amemAttr = attr2, amemVal = val2} =
      obj1 == obj2 && attr1 == attr2 && val1 == val2

-- | Node with Variants
data Node =
  Node
  {
    nodeId :: !ID -- ^ an internal ID

  , nodeParent :: !Node

    -- | children must be a list, because the ordering matters,
    -- e.g. for NCC networks
  , nodeChildren :: !(TList Node)

  , nodeVariant :: !NodeVariant
  }
  |
  DummyTopNode
  {
    -- | children must be a list, because the ordering matters,
    -- e.g. for NCC networks
    nodeChildren :: !(TList Node)

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
    nodeTokens :: !(TSet Token)

    -- | Like in a β memory (see below)
  , bmemAllChildren :: !(TSet Node)
  }
  |
  Bmem
  {
    nodeTokens :: !(TSet Token)

    -- | With left unlinking, we need this list to be able to find and
    -- share also the currently left-unlinked nodes.
  , bmemAllChildren :: !(TSet Node)
  }
  |
  JoinNode
  {
    -- | Points to the α memory this node is attached to.
    nodeAmem :: !Amem
  , nearestAncestorWithSameAmem :: !(Maybe Node)

  , joinTests :: ![JoinTest]

  , leftUnlinked  :: !(TVar Bool)
  , rightUnlinked :: !(TVar Bool)
  }
  |
  NegativeNode
  {
    nodeTokens :: !(TSet Token)

    -- | The α memory this node is attached to (like for JoinNode)
  , nodeAmem :: !Amem

  , joinTests :: ![JoinTest]
  , nearestAncestorWithSameAmem :: !(Maybe Node)

    -- | There is no left unlinking for negative nodes
  , rightUnlinked :: !(TVar Bool)
  }
  |
  NccNode
  {
    nodeTokens :: !(TSet Token)
  , nccPartner :: !Node  -- ^ with NCCPartner variant
  }
  |
  NccPartner
  {
    -- | A corresponding NCC node, must be a TVar because of the
    -- circular dependency. See nccPartner in NCCNode variant.
    nccPartnerNccNode :: !(TVar Node)

  , nccPartnerNumberOfConjucts :: !Int

    -- | Results for the match the NCC node hasn't heard about
  , nccPartnerNewResultBuffer :: !(TSet Token)
  }
  |
  PNode
  {
    nodeTokens :: !(TSet Token)

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
  , joinTestDistance :: !Int
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
                     !Int  -- ^ distance within the token

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

-- | The user-friendly representation of symbols.
data S = S   !String
       | Sym !Symbol

instance Show S where
  show (S   s) = s
  show (Sym s) = show s

-- | The condition of a production.
data Cond =
  -- Positive conds
    PosStr  !String !String !String
  | PosS    !S      !S      !S
  | PosCond !Symbol !Symbol !Symbol -- canonical form

  -- Negative conds
  | NegStr  !String !String !String
  | NegS    !S      !S      !S
  | NegCond !Symbol !Symbol !Symbol -- canonical form

  -- Nccs
  | NccCond ![Cond]

instance Show Cond where
  show (PosStr  o a v) = show o ++ " " ++ show a ++ " " ++ show v
  show (PosS    o a v) = show o ++ " " ++ show a ++ " " ++ show v
  show (PosCond o a v) = show o ++ " " ++ show a ++ " " ++ show v

  show (NegStr  o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  show (NegS    o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  show (NegCond o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v

  show (NccCond conds) = "¬ " ++ show conds
