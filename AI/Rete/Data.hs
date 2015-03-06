{-# LANGUAGE Trustworthy #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Data
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-02
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Data where

import           Control.Monad.Trans.State.Strict (State)
import qualified Data.DList as A
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Int
import           Data.Word

-- IDENTITY

-- | Identifier type. We treat negative identifiers as special ones,
-- and the non-negative as auto-generated.
type Id = Int

-- SYMBOLIC DATA

-- | Type of all values that may be treated as symbolic and that are
-- not supposed to be interned.
data Primitive = BoolPrimitive    !Bool
               | CharPrimitive    !Char
               | DoublePrimitive  !Double
               | FloatPrimitive   !Float
               | IntPrimitive     !Int
               | Int8Primitive    !Int8
               | Int16Primitive   !Int16
               | Int32Primitive   !Int32
               | Int64Primitive   !Int64
               | IntegerPrimitive !Integer
               | WordPrimitive    !Word
               | Word8Primitive   !Word8
               | Word16Primitive  !Word16
               | Word32Primitive  !Word32
               | Word64Primitive  !Word64 deriving Eq

instance Show Primitive where
  show (BoolPrimitive    v) = show v
  show (CharPrimitive    v) = show v
  show (DoublePrimitive  v) = show v
  show (FloatPrimitive   v) = show v
  show (IntPrimitive     v) = show v
  show (Int8Primitive    v) = show v
  show (Int16Primitive   v) = show v
  show (Int32Primitive   v) = show v
  show (Int64Primitive   v) = show v
  show (IntegerPrimitive v) = show v
  show (WordPrimitive    v) = show v
  show (Word8Primitive   v) = show v
  show (Word16Primitive  v) = show v
  show (Word32Primitive  v) = show v
  show (Word64Primitive  v) = show v
  {-# INLINE show #-}

instance Hashable Primitive where
  hashWithSalt salt (BoolPrimitive    v) = salt `hashWithSalt` v
  hashWithSalt salt (CharPrimitive    v) = salt `hashWithSalt` v
  hashWithSalt salt (DoublePrimitive  v) = salt `hashWithSalt` v
  hashWithSalt salt (FloatPrimitive   v) = salt `hashWithSalt` v
  hashWithSalt salt (IntPrimitive     v) = salt `hashWithSalt` v
  hashWithSalt salt (Int8Primitive    v) = salt `hashWithSalt` v
  hashWithSalt salt (Int16Primitive   v) = salt `hashWithSalt` v
  hashWithSalt salt (Int32Primitive   v) = salt `hashWithSalt` v
  hashWithSalt salt (Int64Primitive   v) = salt `hashWithSalt` v
  hashWithSalt salt (IntegerPrimitive v) = salt `hashWithSalt` v
  hashWithSalt salt (WordPrimitive    v) = salt `hashWithSalt` v
  hashWithSalt salt (Word8Primitive   v) = salt `hashWithSalt` v
  hashWithSalt salt (Word16Primitive  v) = salt `hashWithSalt` v
  hashWithSalt salt (Word32Primitive  v) = salt `hashWithSalt` v
  hashWithSalt salt (Word64Primitive  v) = salt `hashWithSalt` v
  {-# INLINE hashWithSalt #-}

-- | Primitive that carries on its textual representation.
data NamedPrimitive =
  NamedPrimitive
  {
    namedPrimitive    :: !Primitive
  , namePrimitiveName :: !String
  }

instance Eq NamedPrimitive where
  NamedPrimitive { namedPrimitive = p1 } ==
    NamedPrimitive { namedPrimitive = p2 } = p1 == p2
  {-# INLINE (==) #-}

instance Show NamedPrimitive where
  show NamedPrimitive { namePrimitiveName = name } = name
  {-# INLINE show #-}

instance Hashable NamedPrimitive where
  hashWithSalt salt NamedPrimitive { namedPrimitive = p } =
    salt `hashWithSalt` p
  {-# INLINE hashWithSalt #-}

-- | Constant (non-variable).
data Constant = StringConstant         !String !Id
              | PrimitiveConstant      !Primitive
              | NamedPrimitiveConstant !NamedPrimitive

instance Show Constant where
  show (StringConstant         s _) = s
  show (PrimitiveConstant      p  ) = show p
  show (NamedPrimitiveConstant np ) = show np
  {-# INLINE show #-}

instance Eq Constant where
  (StringConstant       _ i1 ) == (StringConstant       _ i2 ) = i1  == i2
  (PrimitiveConstant      p1 ) == (PrimitiveConstant      p2 ) = p1  == p2
  (NamedPrimitiveConstant np1) == (NamedPrimitiveConstant np2) = np1 == np2
  _ == _ = False
  {-# INLINE (==) #-}

instance Hashable Constant where
  hashWithSalt salt (StringConstant       _ i ) = salt `hashWithSalt` i
  hashWithSalt salt (PrimitiveConstant      p ) = salt `hashWithSalt` p
  hashWithSalt salt (NamedPrimitiveConstant np) = salt `hashWithSalt` np
  {-# INLINE hashWithSalt #-}

-- | Variable.
data Variable = StringVariable         !String !Id
              | NamedPrimitiveVariable !NamedPrimitive

instance Show Variable where
  show (StringVariable         s _)  = s
  show (NamedPrimitiveVariable np ) = show np
  {-# INLINE show #-}

instance Eq Variable where
  (StringVariable       _ i1 ) == (StringVariable       _ i2 ) = i1  == i2
  (NamedPrimitiveVariable np1) == (NamedPrimitiveVariable np2) = np1 == np2
  _ == _ = False
  {-# INLINE (==) #-}

instance Hashable Variable where
  hashWithSalt salt (StringVariable       _ i ) = salt `hashWithSalt` i
  hashWithSalt salt (NamedPrimitiveVariable np) = salt `hashWithSalt` np
  {-# INLINE hashWithSalt #-}

-- SPECIAL SYMBOLS

emptyConstant :: Constant
emptyConstant =  StringConstant "" (-1)

wildcardConstant :: Constant
wildcardConstant = StringConstant "*" (-3)

-- FIELDS AND THEIR VALUES

-- | Object.
newtype Obj a = Obj a deriving Eq

instance Show a => Show (Obj a) where
  show (Obj s) = show s
  {-# INLINE show #-}

instance Hashable a => Hashable (Obj a) where
  hashWithSalt salt (Obj s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Attribute.
newtype Attr a = Attr a deriving Eq

instance Show a => Show (Attr a) where
  show (Attr s) = show s
  {-# INLINE show #-}

instance Hashable a => Hashable (Attr a) where
  hashWithSalt salt (Attr s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Value.
newtype Val a = Val a deriving Eq

instance Show a => Show (Val a) where
  show (Val s) = show s
  {-# INLINE show #-}

instance Hashable a => Hashable (Val a) where
  hashWithSalt salt (Val s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Field is a description of a location in Wmes, Conds etc. Its
-- variants correspond with Obj, Attr and Val.
data Field = O | A | V deriving (Show, Eq)

instance Hashable Field where
  hashWithSalt salt f = salt `hashWithSalt` case f of
    O -> 1 :: Int
    A -> 2 :: Int
    V -> 3 :: Int
  {-# INLINE hashWithSalt #-}

-- WMES

-- | Working Memory Element. A fact.
data Wme = Wme !(Obj Constant) !(Attr Constant) !(Val Constant) deriving Eq

instance Show Wme where
  show (Wme o a v) = "(" ++ show o  ++ "," ++ show a ++ "," ++ show v ++ ")"
  {-# INLINE show #-}

instance Hashable Wme where
  hashWithSalt salt (Wme o a v) =
    salt `hashWithSalt` o `hashWithSalt` a `hashWithSalt` v
  {-# INLINE hashWithSalt #-}

type WmesIndex a = Map.HashMap a (Set.HashSet Wme     )
type WmesByObj   = WmesIndex     (Obj         Constant)
type WmesByAttr  = WmesIndex     (Attr        Constant)
type WmesByVal   = WmesIndex     (Val         Constant)

-- TOKS (TOKENS)

-- | Token. Represents a series of Wmes matching consecutive
-- conditions in a production.
type Tok = [Wme]

-- | Dummy Top Token - a token with no Wmes.
dtt :: Tok
dtt = []

-- ENVIRONMENT

-- | Working Memory.
data WorkingMemory =
  WorkingMemory
  { reteWmes       :: !(Set.HashSet Wme)
  , reteWmesByObj  :: !WmesByObj
  , reteWmesByAttr :: !WmesByAttr
  , reteWmesByVal  :: !WmesByVal }

-- | Initial, empty working memory.
wmInstance :: WorkingMemory
wmInstance = WorkingMemory { reteWmes       = Set.empty
                           , reteWmesByObj  = Map.empty
                           , reteWmesByAttr = Map.empty
                           , reteWmesByVal  = Map.empty }

-- | Rete is a representation of a network state.
data Rete =
  Rete
  { reteId            :: !Id

  , reteConstants     :: !(Map.HashMap String Constant)
  , reteVariables     :: !(Map.HashMap String Variable)

  , reteWorkingMemory :: !WorkingMemory
  , reteAmems         :: !(Map.HashMap Wme Amem)

  , reteAmemStates    :: !(Map.HashMap Amem AmemState)
  , reteBmemStates    :: !(Map.HashMap Bmem BmemState)
  , reteJoinStates    :: !(Map.HashMap Join JoinState) }

-- | Rete state-monad.
type ReteM a = State Rete a

-- | An initial, empty instance of the Rete network.
reteInstance :: Rete
reteInstance =
  Rete { reteId            = 0
       , reteConstants     = Map.empty
       , reteVariables     = Map.empty
       , reteWorkingMemory = wmInstance
       , reteAmems         = Map.empty
       , reteAmemStates    = Map.empty
       , reteBmemStates    = Map.singleton dtn (BmemState [] [dtt])
       , reteJoinStates    = Map.empty }

-- NETWORK

-- | Alpha memory.
newtype Amem = Amem Id deriving Eq

instance Show Amem where
  show (Amem i) = 'B' : show i
  {-# INLINE show #-}

instance Hashable Amem where
  hashWithSalt salt (Amem i) = salt `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

data AmemState =
  AmemState
  { amemWmes       :: ![Wme]
  , amemWmesByObj  :: !WmesByObj
  , amemWmesByAttr :: !WmesByAttr
  , amemWmesByVal  :: !WmesByVal
  , amemSuccessors :: ![Join] }

-- | Beta memory.
newtype Bmem = Bmem Id deriving Eq

instance Show Bmem where
  show (Bmem i) = 'B' : show i
  {-# INLINE show #-}

instance Hashable Bmem where
  hashWithSalt salt (Bmem i) = salt `hashWithSalt` i
  {-# INLINE hashWithSalt #-}

data BmemState =
  BmemState
  {
    bmemChildren :: ![Join]
  , bmemToks     :: ![Tok]
  }

-- | Dummy Top Node - a Bmem with only Dtt "on board" and (initially) no
-- children.
dtn :: Bmem
dtn = Bmem (-1)

-- | Representation of a join test.
data JoinTest =
  JoinTest
  { joinField1   :: !Field
  , joinField2   :: !Field
  , joinDistance :: !Int } deriving Eq

data Join =
  Join
  { joinId     :: !Id
  , joinTests  :: ![JoinTest]
  , joinAmem   :: !Amem
  , joinParent :: !Bmem }

instance Show Join where
  show join = 'J' : show (joinId join)
  {-# INLINE show #-}

instance Hashable Join where
  hashWithSalt salt join = salt `hashWithSalt` joinId join
  {-# INLINE hashWithSalt #-}

instance Eq Join where
  join1 == join2 = joinId join1 == joinId join2
  {-# INLINE (==) #-}

-- | Join node.
data JoinState =
  JoinState
  { joinChildBmem  :: !(Maybe Bmem)
  , joinChildProds :: ![Prod] }

-- | Production node.
data Prod =
  Prod
  { prodPreds    :: [Pred]
  , prodAction   :: !Action
  , prodBindings :: !Bindings }

-- | A predicate on Toks.
type Pred = Bindings -> Tok -> Bool

-- | Action of a production.
type Action = Bindings -> Tok -> Agenda

-- | Symbol location describes the binding for a variable within a token.
data Location = Location !Int !Field

-- | Map of variable bindings for productions.
type Bindings = Map.HashMap Variable Location

-- | Task to execute on Rete.
data Task = AddWme | AddProd
-- TODO: add details, priorites (see conflict set resolution)

-- | Agenda is a list of Tasks.
type Agenda = A.DList Task

-- CONDITIONS

data ConstantOrVariable = JustConstant !Constant
                        | JustVariable !Variable deriving Eq

instance Show ConstantOrVariable where
  show (JustConstant c) = show c
  show (JustVariable v) = show v
  {-# INLINE show #-}

instance Hashable ConstantOrVariable where
  hashWithSalt salt (JustConstant c) = salt `hashWithSalt` c
  hashWithSalt salt (JustVariable v) = salt `hashWithSalt` v
  {-# INLINE hashWithSalt #-}

-- | Positive Condition.
data Cond = Cond !(Obj  ConstantOrVariable)
                 !(Attr ConstantOrVariable)
                 !(Val  ConstantOrVariable)

instance Show Cond where
  show (Cond o a v) = show o ++ " " ++ show a ++ " " ++ show v
  {-# INLINE show #-}
