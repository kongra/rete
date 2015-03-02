{-# LANGUAGE    Trustworthy #-}
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

-- import           Control.Concurrent.STM (STM, TVar)
-- import qualified Data.HashMap.Strict as Map
-- import qualified Data.HashSet as Set
-- import           Data.Hashable (Hashable, hashWithSalt)
-- import           Data.Int
-- import qualified Data.Sequence as Seq
-- import           Data.Word

import Data.Hashable (Hashable, hashWithSalt)
import Data.Int
import Data.Word

-- IDENTITY

-- | Identifier type. We treat negative identifiers as special ones,
-- and the non-negative as auto-generated.
type Id = Int

-- | Represents types whose elements have Id.
class HavingId a where
  -- | Returns an Id of the argument.
  getId :: a -> Id

-- | Equality operator that uses the Ids of its arguments.
eqOnId :: HavingId a => a -> a -> Bool
obj1 `eqOnId` obj2 = getId obj1 == getId obj2
{-# INLINE eqOnId #-}

-- | Hash with salt implemented with Id of the argument.
hashWithId :: HavingId a => Int -> a -> Int
hashWithId salt x = salt `hashWithSalt` getId x
{-# INLINE hashWithId #-}

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

-- -- | Primitive that carries on its textual representation.
-- data NamedPrimitive = NamedPrimitive !Primitive !String

-- instance Eq NamedPrimitive where
--   (NamedPrimitive p1 _) == (NamedPrimitive p2 _) = p1 == p2
--   {-# INLINE (==) #-}

-- instance Show NamedPrimitive where
--   show (NamedPrimitive _ s) = s
--   {-# INLINE show #-}

-- instance Hashable NamedPrimitive where
--   hashWithSalt salt (NamedPrimitive p _) = salt `hashWithSalt` p
--   {-# INLINE hashWithSalt #-}

-- -- | Constant (non-variable).
-- data Constant = StringConstant         !String !Id
--               | PrimitiveConstant      !Primitive
--               | NamedPrimitiveConstant !NamedPrimitive

-- instance Show Constant where
--   show (StringConstant         s _) = s
--   show (PrimitiveConstant      p  ) = show p
--   show (NamedPrimitiveConstant np ) = show np
--   {-# INLINE show #-}

-- instance Eq Constant where
--   (StringConstant      _  id1) == (StringConstant      _  id2) = id1 == id2
--   (PrimitiveConstant      p1 ) == (PrimitiveConstant      p2 ) = p1  == p2
--   (NamedPrimitiveConstant np1) == (NamedPrimitiveConstant np2) = np1 == np2
--   _ == _ = False
--   {-# INLINE (==) #-}

-- instance Hashable Constant where
--   hashWithSalt salt (StringConstant       _ id') = salt `hashWithSalt` id'
--   hashWithSalt salt (PrimitiveConstant      p  ) = salt `hashWithSalt` p
--   hashWithSalt salt (NamedPrimitiveConstant np ) = salt `hashWithSalt` np
--   {-# INLINE hashWithSalt #-}

-- -- | Variable.
-- data Variable = StringVariable         !String !Id
--               | NamedPrimitiveVariable !NamedPrimitive

-- instance Show Variable where
--   show (StringVariable         s _)  = s
--   show (NamedPrimitiveVariable np ) = show np
--   {-# INLINE show #-}

-- instance Eq Variable where
--   (StringVariable       _ id1) == (StringVariable       _ id2) = id1 == id2
--   (NamedPrimitiveVariable np1) == (NamedPrimitiveVariable np2) = np1 == np2
--   _ == _ = False
--   {-# INLINE (==) #-}

-- instance Hashable Variable where
--   hashWithSalt salt (StringVariable       _ id') = salt `hashWithSalt` id'
--   hashWithSalt salt (NamedPrimitiveVariable np ) = salt `hashWithSalt` np
--   {-# INLINE hashWithSalt #-}

-- -- ENVIRONMENT

-- -- | Environment. Contains a global context for running Rete.
-- data Env =
--   Env
--   {
--     -- | State of the Id generator.
--     envIdState    :: !(TVar Id)

--     -- | Registry of (interned) Constants.
--   , envConstants  :: !(TVar (Map.HashMap String Constant))

--     -- | Registry of (interned) Variables.
--   , envVariables  :: !(TVar (Map.HashMap String Variable))

--     -- | All Wmes indexed by their WmeKey.
--   , envWmes       :: !(TVar (Map.HashMap WmeKey Wme))

--     -- 3 Wme indexes by Wme Field value
--   , envWmesByObj  :: !(TVar WmesByObj )
--   , envWmesByAttr :: !(TVar WmesByAttr)
--   , envWmesByVal  :: !(TVar WmesByVal )

--     -- | Known alpha memories indexed by their WmeKey.
--   , envAmems      :: !(TVar (Map.HashMap WmeKey Amem))

--     -- | Productions the Env knows about.
--   , envProds      :: !(TVar (Set.HashSet Prod))

--     -- | The Dummy Top Node.
--   , envDtn        :: !Dtn
--   }

-- -- FIELDS AND THEIR VALUES

-- -- | Object.
-- newtype Obj a = Obj a deriving Eq

-- instance Show a => Show (Obj a) where
--   show (Obj s) = show s
--   {-# INLINE show #-}

-- instance Hashable a => Hashable (Obj a) where
--   hashWithSalt salt (Obj s) = salt `hashWithSalt` s
--   {-# INLINE hashWithSalt #-}

-- -- | Attribute.
-- newtype Attr a = Attr a deriving Eq

-- instance Show a => Show (Attr a) where
--   show (Attr s) = show s
--   {-# INLINE show #-}

-- instance Hashable a => Hashable (Attr a) where
--   hashWithSalt salt (Attr s) = salt `hashWithSalt` s
--   {-# INLINE hashWithSalt #-}

-- -- | Value.
-- newtype Val a = Val a deriving Eq

-- instance Show a => Show (Val a) where
--   show (Val s) = show s
--   {-# INLINE show #-}

-- instance Hashable a => Hashable (Val a) where
--   hashWithSalt salt (Val s) = salt `hashWithSalt` s
--   {-# INLINE hashWithSalt #-}

-- -- | Field is a description of a location in Wmes, Conds etc. Its
-- -- variants correspond with Obj, Attr and Val.
-- data Field = O | A | V deriving (Show, Eq)

-- instance Hashable Field where
--   hashWithSalt salt f = salt `hashWithSalt` case f of
--     O -> 1 :: Int
--     A -> 2 :: Int
--     V -> 3 :: Int
--   {-# INLINE hashWithSalt #-}

-- -- WMES

-- type WmesIndex a = Map.HashMap a (Set.HashSet Wme     )
-- type WmesByObj   = WmesIndex     (Obj         Constant)
-- type WmesByAttr  = WmesIndex     (Attr        Constant)
-- type WmesByVal   = WmesIndex     (Val         Constant)

-- -- | Working Memory Element (fact).
-- data Wme =
--   Wme
--   {
--     wmeId :: !Id

--   , wmeObj  :: !(Obj  Constant)
--   , wmeAttr :: !(Attr Constant)
--   , wmeVal  :: !(Val  Constant)

--     -- | Amems this Wme belongs to (8 at most).
--   , wmeAmems :: !(TVar [Amem])

--     -- | Toks with tokenWme = this Wme.
--   , wmeToks :: !(TVar (Set.HashSet WmeTok))

--     -- | Negative join results in which this Wme participates.
--   , wmeNegJoinResults :: !(TVar (Set.HashSet NegJoinResult))
--   }

-- instance Show Wme where
--   show Wme { wmeObj = obj, wmeAttr = attr, wmeVal = val } =
--     "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"
--   {-# INLINE show #-}

-- instance HavingId Wme where
--   getId = wmeId
--   {-# INLINE getId #-}

-- instance Eq Wme where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Wme where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Key for a Wme.
-- data WmeKey = WmeKey !(Obj Constant) !(Attr Constant) !(Val Constant) deriving Eq

-- instance Hashable WmeKey where
--   hashWithSalt salt (WmeKey obj attr val) =
--     salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val
--   {-# INLINE hashWithSalt #-}

-- -- TOKENS

-- -- | Dummy Top Token.
-- data Dtt = Dtt

-- -- | Beta memory token.
-- data Btok =
--   Btok
--   {
--     btokId       :: !Id
--   , btokWme      :: !Wme
--   , btokParent   :: !(Either Dtt Btok)
--   , btokNode     :: !Bmem
--   , btokChildren :: !(TVar (Set.HashSet WmeTok))
--   }

-- instance HavingId Btok where
--   getId = btokId
--   {-# INLINE getId #-}

-- instance Eq Btok where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Btok where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Negation node token.
-- data Ntok =
--   Ntok
--   {
--     ntokId             :: !Id
--   , ntokWme            :: !(Maybe Wme)
--   , ntokParent         :: !(Either JoinTok Ntok)
--   , ntokNode           :: !Neg
--   , ntokChildren       :: !(TVar (Set.HashSet (Either Ntok Ptok)))
--   , ntokNegJoinResults :: !(TVar (Set.HashSet NegJoinResult))
--   }

-- instance HavingId Ntok where
--   getId = ntokId
--   {-# INLINE getId #-}

-- instance Eq Ntok where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Ntok where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Negative join result.
-- data NegJoinResult =
--   NegJoinResult { njrOwner :: !Ntok
--                 , njrWme   :: !Wme } deriving Eq

-- instance Hashable NegJoinResult where
--   hashWithSalt salt (NegJoinResult owner wme) =
--     salt `hashWithSalt` owner `hashWithSalt` wme
--   {-# INLINE hashWithSalt #-}

-- -- | Production node token.
-- data Ptok =
--   Ptok
--   {
--     ptokId     :: !Id
--   , ptokWme    :: !(Maybe Wme)
--   , ptokParent :: !(Either JoinTok Ntok)
--   , ptokNode   :: !Prod
--   }

-- instance HavingId Ptok where
--   getId = ptokId
--   {-# INLINE getId #-}

-- instance Eq Ptok where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Ptok where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | A token potentially holding a Wme.
-- data WmeTok = BmemWmeTok !Btok
--             | NegWmeTok  !Ntok
--             | ProdWmeTok !Ptok deriving Eq

-- instance Hashable WmeTok where
--   hashWithSalt salt tok = case tok of
--     BmemWmeTok btok -> salt `hashWithSalt` btok
--     NegWmeTok  ntok -> salt `hashWithSalt` ntok
--     ProdWmeTok ptok -> salt `hashWithSalt` ptok
--   {-# INLINE hashWithSalt #-}

-- -- | Type of tokens propagated by Joins.
-- type JoinTok = Either Dtt Btok

-- -- ALPHA MEMORY

-- -- | Alpha Memory.
-- data Amem =
--   Amem
--   {
--     -- | Successors must be a list, cause the ordering matters.
--     amemSuccessors :: !(TVar (Seq.Seq AmemSuccessor))

--     -- | The number of join or negative nodes using this Amem.
--   , amemRefCount   :: !(TVar Int)

--     -- | The wmes in this Amem (unindexed).
--   , amemWmes       :: !(TVar (Set.HashSet Wme))

--     -- | Wmes are indexed by their Field value.
--   , amemWmesByObj  :: !(TVar WmesByObj)
--   , amemWmesByAttr :: !(TVar WmesByAttr)
--   , amemWmesByVal  :: !(TVar WmesByVal)

--     -- Keys to identify the α memory in the α memories registry.
--   , amemObj        :: !(Obj  Constant)
--   , amemAttr       :: !(Attr Constant)
--   , amemVal        :: !(Val  Constant)
--   }

-- instance Eq Amem where
--   Amem   { amemObj = obj1, amemAttr = attr1, amemVal = val1 } ==
--     Amem { amemObj = obj2, amemAttr = attr2, amemVal = val2 } =
--       obj1 == obj2 && attr1 == attr2 && val1 == val2
--   {-# INLINE (==) #-}

-- instance Hashable Amem where
--   hashWithSalt salt Amem { amemObj = obj, amemAttr = attr, amemVal = val } =
--     salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val
--   {-# INLINE hashWithSalt #-}

-- -- | Amem successor. May also be used to represent nodes holding a
-- -- reference to an Amem.
-- data AmemSuccessor = JoinSuccessor !Join
--                    | NegSuccessor  !Neg deriving Eq

-- type AmemSuccessorsMap a = Map.HashMap AmemSuccessorKey a

-- -- BETA NETWORK NODES

-- -- | Dummy Top Node
-- data Dtn =
--   Dtn
--   {
--     dtnAllChildren :: !(TVar (AmemSuccessorsMap Join))
--   }

-- -- | Beta Memory.
-- data Bmem =
--   Bmem
--   {
--     bmemId          :: !Id
--   , bmemParent      :: !Join
--   , bmemChildren    :: !(TVar (Set.HashSet       Join))
--   , bmemAllChildren :: !(TVar (AmemSuccessorsMap Join))
--   , bmemToks        :: !(TVar (Set.HashSet       Btok))
--   }

-- instance HavingId Bmem where
--   getId = bmemId
--   {-# INLINE getId #-}

-- instance Eq Bmem where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Bmem where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Representation of a join test.
-- data JoinTest =
--   JoinTest
--   {
--     joinField1   :: !Field
--   , joinField2   :: !Field
--   , joinDistance :: !Int
--   }
--   deriving Eq

-- instance Hashable JoinTest where
--   hashWithSalt salt JoinTest { joinField1   = f1,
--                                joinField2   = f2,
--                                joinDistance = d } =
--     salt `hashWithSalt` f1 `hashWithSalt` f2 `hashWithSalt` d
--   {-# INLINE hashWithSalt #-}

-- -- | A key to fast search for AmemSuccessors in their parent nodes
-- -- (during network creation).
-- data AmemSuccessorKey = AmemSuccessorKey !Amem ![JoinTest] deriving Eq

-- instance Hashable AmemSuccessorKey where
--   hashWithSalt salt (AmemSuccessorKey amem tests) =
--     salt `hashWithSalt` amem `hashWithSalt` tests

-- -- | Join node.
-- data Join =
--   Join
--   {
--     joinId              :: !Id
--   , joinParent          :: !(Either Dtn Bmem)

--     -- Join may have at most 1 Bmem child.
--   , joinBmem            :: !(TVar (Maybe             Bmem))
--   , joinNegs            :: !(TVar (AmemSuccessorsMap Neg ))
--   , joinProds           :: !(TVar (Set.HashSet       Prod))

--   , joinAmem            :: !Amem
--   , joinNearestAncestor :: !(Maybe Join)
--   , joinTests           :: ![JoinTest]
--   , joinLeftUnlinked    :: !(TVar Bool)
--   , joinRightUnlinked   :: !(TVar Bool)
--   }

-- instance HavingId Join where
--   getId = joinId
--   {-# INLINE getId #-}

-- instance Eq Join where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Join where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Negative Node.
-- data Neg =
--   Neg
--   {
--     negId              :: !Id
--   , negParent          :: !(Either Join Neg)

--   , negNegs            :: !(TVar (AmemSuccessorsMap Neg))
--   , negProds           :: !(TVar (Set.HashSet       Prod))

--   , negToks            :: !(TVar (Set.HashSet Ntok))
--   , negAmem            :: !Amem
--   , negTests           :: ![JoinTest]
--   , negNearestAncestor :: !(Maybe AmemSuccessor)
--   , negRightUnlinked   :: !(TVar Bool)
--   }

-- instance HavingId Neg where
--   getId = negId
--   {-# INLINE getId #-}

-- instance Eq Neg where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Neg where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- | Symbol location describes the binding for a variable within a token.
-- data Location = Location !Int !Field

-- -- | Map of variable bindings for productions.
-- type Bindings = Map.HashMap Variable Location

-- -- | Production node.
-- data Prod =
--   Prod
--   {
--     prodId           :: !Id
--   , prodParent       :: !(Either Join Neg)
--   , prodToks         :: !(TVar (Set.HashSet Ptok))
--   , prodAction       :: !Action
--   , prodRevokeAction :: !(Maybe Action)
--   , prodBindings     :: !Bindings
--   }

-- instance HavingId Prod where
--   getId = prodId
--   {-# INLINE getId #-}

-- instance Eq Prod where
--   (==) = eqOnId
--   {-# INLINE (==) #-}

-- instance Hashable Prod where
--   hashWithSalt = hashWithId
--   {-# INLINE hashWithSalt #-}

-- -- ACTIONS

-- -- | Context of a production action.
-- data Actx =
--   Actx
--   {
--     actxEnv  :: !Env         -- ^ Current Env
--   , actxProd :: !Prod        -- ^ Production node
--   , actxTok  :: !Ptok        -- ^ The matching token
--   , actxWmes :: ![Maybe Wme] -- ^ Wmes of the matching token
--   }

-- -- | Action of a production.
-- type Action = Actx -> STM ()

-- -- CONDITIONS

-- data ConstantOrVariable = JustConstant !Constant
--                         | JustVariable !Variable deriving Eq

-- instance Show ConstantOrVariable where
--   show (JustConstant c) = show c
--   show (JustVariable v) = show v
--   {-# INLINE show #-}

-- instance Hashable ConstantOrVariable where
--   hashWithSalt salt (JustConstant c) = salt `hashWithSalt` c
--   hashWithSalt salt (JustVariable v) = salt `hashWithSalt` v
--   {-# INLINE hashWithSalt #-}

-- data PosCond = PosCond !(Obj  ConstantOrVariable)
--                        !(Attr ConstantOrVariable)
--                        !(Val  ConstantOrVariable)

-- instance Show PosCond where
--   show (PosCond o a v) = show o ++ " " ++ show a ++ " " ++ show v
--   {-# INLINE show #-}

-- data NegCond = NegCond !(Obj  ConstantOrVariable)
--                        !(Attr ConstantOrVariable)
--                        !(Val  ConstantOrVariable)

-- instance Show NegCond where
--   show (NegCond o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
--   {-# INLINE show #-}
