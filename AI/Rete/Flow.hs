{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Flow
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-03
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Flow
    (
      -- * Adding Wmes
      addWme
    , addWmeP

      -- * Variables
      , Var
      , var

      -- * Internals
    , genid
    , wmesIndexInsert
    , rightActivateJoin
    , ToConstantOrVariable (..)
    , fieldConstant
    )
    where

import           AI.Rete.Data
import           AI.Rete.State
import           Control.Monad (when, liftM, liftM3, forM)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.Maybe (isNothing)
import           Data.Word
import           Kask.Control.Lens (view, set, over)
import           Kask.Data.List (nthDef)

-- | Generates a new Id.
genid :: ReteM Id
genid = do
  state <- viewS Rete
  let recent = view reteId state
  when (recent == maxBound) (error "PANIC (2): Id OVERFLOW.")

  let new = recent + 1
  setS Rete (set reteId new state)
  return new

-- INTERNING CONSTANTS AND VARIABLES

internConstant :: String -> ReteM Constant
internConstant s = do
  cs <- liftM (view reteConstants) (viewS Rete)
  case Map.lookup s cs of
    Just c  -> return c
    Nothing -> do
      i    <- genid
      let c = StringConstant s i
      overS (over reteConstants (Map.insert s c)) Rete
      return c

internVariable :: String -> ReteM Variable
internVariable s = do
  vs <- liftM (view reteVariables) (viewS Rete)
  case Map.lookup s vs of
    Just v  -> return v
    Nothing -> do
      i    <- genid
      let v = StringVariable s i
      overS (over reteVariables (Map.insert s v)) Rete
      return v

internFields :: (ToConstant o, ToConstant a, ToConstant v)
             => o -> a -> v
             -> ReteM (Obj Constant, Attr Constant, Val Constant)
internFields o a v =
  liftM3 (,,) (internField Obj o) (internField Attr a) (internField Val v)

internField :: ToConstant a => (Constant -> b) -> a -> ReteM b
internField f s = liftM f (toConstant s)

-- WMES INDEXES MANIPULATION

type WmesIndexOperator a =
  (Hashable a, Eq a) => a -> Wme -> WmesIndex a -> WmesIndex a

-- | Creates an updated version of the wme index by putting a new
-- wme under the key k.
wmesIndexInsert ::  WmesIndexOperator a
wmesIndexInsert k wme index = Map.insert k (Set.insert wme s) index
  where s  = Map.lookupDefault Set.empty k index

addToWorkingMemory :: Wme -> ReteM ()
addToWorkingMemory wme@(Wme o a v) =
  overS
  (  over reteWmes       (Set.insert        wme)
   . over reteWmesByObj  (wmesIndexInsert o wme)
   . over reteWmesByAttr (wmesIndexInsert a wme)
   . over reteWmesByVal  (wmesIndexInsert v wme)) Rete

-- ALPHA MEMORY

activateAmem :: Amem -> Wme -> ReteM Agenda
activateAmem amem wme@(Wme o a v) = do
  state <- viewS amem
  setS amem $ (  over amemWmes       (wme:)
               . over amemWmesByObj  (wmesIndexInsert o wme)
               . over amemWmesByAttr (wmesIndexInsert a wme)
               . over amemWmesByVal  (wmesIndexInsert v wme)) state

  agendas <- mapM (rightActivateJoin wme) (view amemSuccessors state)
  return (concat agendas)

feedAmem :: Map.HashMap Wme Amem -> Wme -> Wme -> ReteM Agenda
feedAmem amems wme k = case Map.lookup k amems of
  Just amem -> activateAmem amem wme
  Nothing   -> return []

feedAmems :: Wme -> Obj Constant -> Attr Constant -> Val Constant -> ReteM Agenda
feedAmems wme o a v = do
  let w = wildcardConstant
  amems <- liftM (view reteAmems) (viewS Rete)

  a1 <- feedAmem amems wme $! Wme      o        a       v
  a2 <- feedAmem amems wme $! Wme      o        a  (Val w)
  a3 <- feedAmem amems wme $! Wme      o  (Attr w)      v
  a4 <- feedAmem amems wme $! Wme      o  (Attr w) (Val w)

  a5 <- feedAmem amems wme $! Wme (Obj w)       a       v
  a6 <- feedAmem amems wme $! Wme (Obj w)       a  (Val w)
  a7 <- feedAmem amems wme $! Wme (Obj w) (Attr w)      v
  a8 <- feedAmem amems wme $! Wme (Obj w) (Attr w) (Val w)

  return $ a1 ++ a2 ++ a3 ++ a4 ++ a5 ++ a6 ++ a7 ++ a8

-- BETA MEMORY

leftActivateBmem :: Bmem -> Tok -> Wme -> ReteM Agenda
leftActivateBmem bmem tok wme = do
  let newTok = wme:tok
  state <- viewS bmem
  setS bmem $ over bmemToks (newTok:) state

  agendas <- mapM (leftActivateJoin newTok) (view bmemChildren state)
  return (concat agendas)

-- UNINDEXED JOIN

performJoinTests :: [JoinTest] -> Tok -> Wme -> Bool
performJoinTests tests tok wme = all (passJoinTest tok wme) tests

passJoinTest :: Tok -> Wme -> JoinTest -> Bool
passJoinTest tok wme
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    fieldConstant f1 wme == fieldConstant f2 wme2
    where
      wme2  = nthDef (error ("PANIC (3): ILLEGAL INDEX " ++ show d)) d tok

fieldConstant :: Field -> Wme -> Constant
fieldConstant O (Wme (Obj c)       _       _)  = c
fieldConstant A (Wme _       (Attr c)      _)  = c
fieldConstant V (Wme _             _  (Val c)) = c

-- INDEXED JOIN

matchingAmemWmes :: [JoinTest] -> Tok -> AmemState -> [Wme]
matchingAmemWmes []    _   amemState = toList (view amemWmes amemState)
matchingAmemWmes tests tok amemState =  -- At least one test specified.
  toList (foldr Set.intersection s sets)
  where
    (s:sets) = map (amemWmesForTest tok amemState) tests

amemWmesForTest :: [Wme] -> AmemState -> JoinTest -> Set.HashSet Wme
amemWmesForTest wmes amemState
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    case f1 of
      O -> amemWmesForIndex (Obj  c) (view amemWmesByObj  amemState)
      A -> amemWmesForIndex (Attr c) (view amemWmesByAttr amemState)
      V -> amemWmesForIndex (Val  c) (view amemWmesByVal  amemState)
    where
      wme = nthDef (error ("PANIC (4): ILLEGAL INDEX " ++ show d)) d wmes
      c   = fieldConstant f2 wme

amemWmesForIndex :: (Hashable a, Eq a) => a -> WmesIndex a -> Set.HashSet Wme
amemWmesForIndex = Map.lookupDefault Set.empty

-- JOIN

rightActivateJoin :: Wme -> Join -> ReteM Agenda
rightActivateJoin wme join = do
  state   <- viewS join
  toks    <- liftM (view bmemToks) (viewS (joinParent join))
  agendas <- forM toks $ \tok ->
    if performJoinTests (joinTests join) tok wme
      then leftActivateJoinChildren state tok wme
      else return []

  return (concat agendas)

leftActivateJoin :: Tok -> Join -> ReteM Agenda
leftActivateJoin tok join = do
  state <- viewS join
  if noJoinChildren state
    then return []
    else do
      amemState <- viewS (joinAmem join)
      let wmes = matchingAmemWmes (joinTests join) tok amemState
      agendas <- forM wmes $ \wme -> leftActivateJoinChildren state tok wme
      return (concat agendas)

leftActivateJoinChildren :: JoinState -> Tok -> Wme -> ReteM Agenda
leftActivateJoinChildren state tok wme = do
  agenda <- case view joinChildBmem state of
    Just bmem -> leftActivateBmem bmem tok wme
    Nothing   -> return []

  agendas <- mapM (leftActivateProd tok wme) (view joinChildProds state)
  return (concat (agenda : agendas))

noJoinChildren :: JoinState -> Bool
noJoinChildren state =
  isNothing (view joinChildBmem state) && null (view joinChildProds state)

-- PROD

leftActivateProd :: Tok -> Wme -> Prod -> ReteM Agenda
leftActivateProd tok wme prod@Prod { prodPreds    = preds
                                   , prodAction   = action }  = do
  let newTok     = wme:tok
      actx       = Actx prod newTok
      matching p = p actx

  if all matching preds
    then return (map withThisProd (action actx))
    else return []

  where withThisProd task = task { taskProd = Just prod }

-- ADDING WMES

-- | Creates a task with default priority (0) that represents adding a Wme.
addWme :: (ToConstant o, ToConstant a, ToConstant v) => o -> a -> v -> Task
addWme o a v = addWmeP o a v 0

-- | Creates a task with given priority that represents adding a Wme.
addWmeP :: (ToConstant o, ToConstant a, ToConstant v)
        => o -> a -> v -> Int -> Task
addWmeP o a v priority = Task (addWmeA o a v) priority Nothing

-- | Creates the Agenda in Rete monad that represents adding a Wme.
addWmeA :: (ToConstant o, ToConstant a, ToConstant v) => o -> a -> v
        -> ReteM Agenda
addWmeA o a v = do
  (o', a', v') <- internFields o a v
  let wme = Wme o' a' v'
  state <- viewS Rete
  if Set.member wme (view reteWmes state)
    then return [] -- Already present, do nothing.
    else do
      addToWorkingMemory wme
      feedAmems wme o' a' v'

-- INTERNING PRIMITIVES

-- | Represents a constant at the system level.
class ToConstant a where
  -- | Interns and returns a Symbol for the name argument.
  toConstant :: a -> ReteM Constant

instance ToConstant Constant where
  -- We may simply return the argument here, because Constants once
  -- interned never expire (get un-interned).
  toConstant = return

instance ToConstant Primitive where
  -- Every Primitive is treated as a Const.
  toConstant = return . PrimitiveConstant

instance ToConstant Bool where
  toConstant = toConstant . BoolPrimitive

instance ToConstant Char where
  toConstant = toConstant . CharPrimitive

instance ToConstant Double where
  toConstant = toConstant . DoublePrimitive

instance ToConstant Float where
  toConstant = toConstant . FloatPrimitive

instance ToConstant Int where
  toConstant = toConstant . IntPrimitive

instance ToConstant Int8 where
  toConstant = toConstant . Int8Primitive

instance ToConstant Int16 where
  toConstant = toConstant . Int16Primitive

instance ToConstant Int32 where
  toConstant = toConstant . Int32Primitive

instance ToConstant Int64 where
  toConstant = toConstant . Int64Primitive

instance ToConstant Integer where
  toConstant = toConstant . IntegerPrimitive

instance ToConstant Word where
  toConstant = toConstant . WordPrimitive

instance ToConstant Word8 where
  toConstant = toConstant . Word8Primitive

instance ToConstant Word16 where
  toConstant = toConstant . Word16Primitive

instance ToConstant Word32 where
  toConstant = toConstant . Word32Primitive

instance ToConstant Word64  where
  toConstant = toConstant . Word64Primitive

instance ToConstant String where
  -- Raw String is always a constant.
  toConstant ""   = return emptyConstant
  toConstant name = internConstant name

instance ToConstant NamedPrimitive where
  toConstant = return . NamedPrimitiveConstant

-- EXPLICIT CONSTRUCTORS FOR VARIABLES

type Var = ReteM Variable

-- | A type of values with a variable semantics.
class ToVar a where
  -- | Marks a thing as a variable resulting in a Symbolic value.
  var :: a -> Var

instance ToVar String where
  var "" = error "ERROR (1): EMPTY VARIABLE NAME."
  var s  = internVariable s

instance ToVar NamedPrimitive where
  var (NamedPrimitive _ "") = error "ERROR (2): EMPTY VARIABLE NAME."
  var np                    = return (NamedPrimitiveVariable np)

instance ToVar Var where
  var = id

class ToConstantOrVariable a where
  toConstantOrVariable :: a -> ReteM ConstantOrVariable

instance ToConstantOrVariable Var where
  toConstantOrVariable = liftM JustVariable

instance ToConstantOrVariable Primitive where
  -- Every Primitive is treated as a Const.
  toConstantOrVariable = return . JustConstant . PrimitiveConstant

instance ToConstantOrVariable Bool where
  toConstantOrVariable = toConstantOrVariable . BoolPrimitive

instance ToConstantOrVariable Char where
  toConstantOrVariable = toConstantOrVariable . CharPrimitive

instance ToConstantOrVariable Double where
  toConstantOrVariable = toConstantOrVariable . DoublePrimitive

instance ToConstantOrVariable Float where
  toConstantOrVariable = toConstantOrVariable . FloatPrimitive

instance ToConstantOrVariable Int where
  toConstantOrVariable = toConstantOrVariable . IntPrimitive

instance ToConstantOrVariable Int8 where
  toConstantOrVariable = toConstantOrVariable . Int8Primitive

instance ToConstantOrVariable Int16 where
  toConstantOrVariable = toConstantOrVariable . Int16Primitive

instance ToConstantOrVariable Int32 where
  toConstantOrVariable = toConstantOrVariable . Int32Primitive

instance ToConstantOrVariable Int64 where
  toConstantOrVariable = toConstantOrVariable . Int64Primitive

instance ToConstantOrVariable Integer where
  toConstantOrVariable = toConstantOrVariable . IntegerPrimitive

instance ToConstantOrVariable Word where
  toConstantOrVariable = toConstantOrVariable . WordPrimitive

instance ToConstantOrVariable Word8 where
  toConstantOrVariable = toConstantOrVariable . Word8Primitive

instance ToConstantOrVariable Word16 where
  toConstantOrVariable = toConstantOrVariable . Word16Primitive

instance ToConstantOrVariable Word32 where
  toConstantOrVariable = toConstantOrVariable . Word32Primitive

instance ToConstantOrVariable Word64  where
  toConstantOrVariable = toConstantOrVariable . Word64Primitive

instance ToConstantOrVariable String where
  -- Raw String is always a constant.
  toConstantOrVariable "" = return (JustConstant emptyConstant)
  toConstantOrVariable s  = liftM JustConstant (internConstant s)

instance ToConstantOrVariable NamedPrimitive where
  toConstantOrVariable = return . JustConstant . NamedPrimitiveConstant
