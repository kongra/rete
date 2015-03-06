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
module AI.Rete.Flow where

import           AI.Rete.Data
import           Control.Monad (when, liftM, liftM3, forM)
import           Control.Monad.Trans.State.Strict (get, put)
import qualified Data.DList as A
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.Maybe (isNothing)
import           Data.Word
import           Kask.Data.List (nthDef)

-- | Generates a new Id.
genid :: ReteM Id
genid = do
  rete <- get
  let recent = reteId rete
  when (recent == maxBound) (error "PANIC (1): Id OVERFLOW.")

  let new = recent + 1
  put rete { reteId = new }
  return new
{-# INLINE genid #-}

-- INTERNING CONSTANTS AND VARIABLES

internConstant :: String -> ReteM Constant
internConstant s = do
  cs <- liftM reteConstants get
  case Map.lookup s cs of
    Just c  -> return c
    Nothing -> do
      i    <- genid
      rete <- get
      let c = StringConstant s i
      put rete { reteConstants = Map.insert s c cs }
      return c
{-# INLINE internConstant #-}

internVariable :: String -> ReteM Variable
internVariable s = do
  vs <- liftM reteVariables get
  case Map.lookup s vs of
    Just v  -> return v
    Nothing -> do
      i    <- genid
      rete <- get
      let v = StringVariable s i
      put rete { reteVariables = Map.insert s v vs }
      return v
{-# INLINE internVariable #-}

internFields :: (ToConstant o, ToConstant a, ToConstant v)
             => o -> a -> v
             -> ReteM (Obj Constant, Attr Constant, Val Constant)
internFields o a v =
  liftM3 (,,) (internField Obj o) (internField Attr a) (internField Val v)
{-# INLINE internFields #-}

internField :: ToConstant a => (Constant -> b) -> a -> ReteM b
internField f s = liftM f (toConstant s)
{-# INLINE internField #-}

-- WMES INDEXES MANIPULATION

type WmesIndexOperator a =
  (Hashable a, Eq a) => a -> Wme -> WmesIndex a -> WmesIndex a

-- | Creates an updated version of the wme index by putting a new
-- wme under the key k.
wmesIndexInsert ::  WmesIndexOperator a
wmesIndexInsert k wme index = Map.insert k (Set.insert wme s) index
  where s  = Map.lookupDefault Set.empty k index
{-# INLINE wmesIndexInsert #-}

addToWorkingMemory :: Wme -> ReteM ()
addToWorkingMemory wme@(Wme o a v) = do
  rete <- get
  let wmem   = reteWorkingMemory rete
      wmes   = reteWmes          wmem
      byObj  = reteWmesByObj     wmem
      byAttr = reteWmesByAttr    wmem
      byVal  = reteWmesByVal     wmem

  put rete { reteWorkingMemory =
                wmem { reteWmes       = Set.insert        wme wmes
                     , reteWmesByObj  = wmesIndexInsert o wme byObj
                     , reteWmesByAttr = wmesIndexInsert a wme byAttr
                     , reteWmesByVal  = wmesIndexInsert v wme byVal }}
{-# INLINE addToWorkingMemory #-}

-- ALPHA MEMORY

activateAmem :: Amem -> Wme -> ReteM Agenda
activateAmem amem wme@(Wme o a v) = do
  rete <- get
  let amemStates = reteAmemStates rete
      state = Map.lookupDefault
              (error ("PANIC (2): STATE NOT FOUND FOR " ++ show amem))
              amem amemStates

      newState =
        state { amemWmes       = wme : amemWmes state
              , amemWmesByObj  = wmesIndexInsert o wme (amemWmesByObj  state)
              , amemWmesByAttr = wmesIndexInsert a wme (amemWmesByAttr state)
              , amemWmesByVal  = wmesIndexInsert v wme (amemWmesByVal  state) }

  put rete { reteAmemStates = Map.insert amem newState amemStates }

  agendas <- mapM (rightActivateJoin wme) (amemSuccessors newState)
  return (A.concat agendas)
{-# INLINE activateAmem #-}

feedAmem :: Map.HashMap Wme Amem -> Wme -> Wme -> ReteM Agenda
feedAmem amems wme k = case Map.lookup k amems of
  Just amem -> activateAmem amem wme
  Nothing   -> return A.empty
{-# INLINE feedAmem #-}

feedAmems :: Wme -> Obj Constant -> Attr Constant -> Val Constant -> ReteM Agenda
feedAmems wme o a v = do
  let w = wildcardConstant
  amems <- liftM reteAmems get

  a1 <- feedAmem amems wme $! Wme      o        a       v
  a2 <- feedAmem amems wme $! Wme      o        a  (Val w)
  a3 <- feedAmem amems wme $! Wme      o  (Attr w)      v
  a4 <- feedAmem amems wme $! Wme      o  (Attr w) (Val w)

  a5 <- feedAmem amems wme $! Wme (Obj w)       a       v
  a6 <- feedAmem amems wme $! Wme (Obj w)       a  (Val w)
  a7 <- feedAmem amems wme $! Wme (Obj w) (Attr w)      v
  a8 <- feedAmem amems wme $! Wme (Obj w) (Attr w) (Val w)

  return $
    a1 `A.append`
    a2 `A.append`
    a3 `A.append`
    a4 `A.append`
    a5 `A.append`
    a6 `A.append`
    a7 `A.append`
    a8
{-# INLINE feedAmems #-}

-- BETA MEMORY

leftActivateBmem :: Bmem -> Tok -> Wme -> ReteM Agenda
leftActivateBmem bmem tok wme = do
  rete <- get
  let newTok     = wme:tok
      bmemStates = reteBmemStates rete
      state      = Map.lookupDefault
                   (error ("PANIC (3): STATE NOT FOUND FOR " ++ show bmem))
                   bmem bmemStates
      newState  = state { bmemToks = newTok : bmemToks state }

  put rete { reteBmemStates = Map.insert bmem newState bmemStates }

  agendas <- mapM (leftActivateJoin newTok) (bmemChildren newState)
  return (A.concat agendas)
{-# INLINE leftActivateBmem #-}

-- UNINDEXED JOIN

performJoinTests :: [JoinTest] -> Tok -> Wme -> Bool
performJoinTests tests tok wme = all (passJoinTest tok wme) tests
{-# INLINE performJoinTests #-}

passJoinTest :: Tok -> Wme -> JoinTest -> Bool
passJoinTest tok wme
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    fieldConstant f1 wme == fieldConstant f2 wme2
    where
      wme2  = nthDef (error ("PANIC (4): ILLEGAL INDEX " ++ show d)) d tok
{-# INLINE passJoinTest #-}

fieldConstant :: Field -> Wme -> Constant
fieldConstant O (Wme (Obj c)       _       _)  = c
fieldConstant A (Wme _       (Attr c)      _)  = c
fieldConstant V (Wme _             _  (Val c)) = c
{-# INLINE fieldConstant #-}

-- INDEXED JOIN

matchingAmemWmes :: [JoinTest] -> Tok -> AmemState -> [Wme]
matchingAmemWmes []    _   amemState = toList (amemWmes amemState)
matchingAmemWmes tests tok amemState =  -- At least one test specified.
  toList (foldr Set.intersection s sets)
  where
    (s:sets) = map (amemWmesForTest tok amemState) tests
{-# INLINE matchingAmemWmes #-}

amemWmesForTest :: [Wme] -> AmemState -> JoinTest -> Set.HashSet Wme
amemWmesForTest wmes amemState
  JoinTest { joinField1 = f1, joinField2 = f2, joinDistance = d } =
    case f1 of
      O -> amemWmesForIndex (Obj  c) (amemWmesByObj  amemState)
      A -> amemWmesForIndex (Attr c) (amemWmesByAttr amemState)
      V -> amemWmesForIndex (Val  c) (amemWmesByVal  amemState)
    where
      wme = nthDef (error ("PANIC (5): ILLEGAL INDEX " ++ show d)) d wmes
      c   = fieldConstant f2 wme
{-# INLINE amemWmesForTest #-}

amemWmesForIndex :: (Hashable a, Eq a) => a -> WmesIndex a -> Set.HashSet Wme
amemWmesForIndex = Map.lookupDefault Set.empty
{-# INLINE amemWmesForIndex #-}

-- JOIN

rightActivateJoin :: Wme -> Join -> ReteM Agenda
rightActivateJoin wme join = do
  rete <- get
  let bmemStates  = reteBmemStates rete
      joinStates  = reteJoinStates rete

      parent      = joinParent join
      parentState = Map.lookupDefault
                    (error ("PANIC (6): STATE NOT FOUND FOR " ++ show parent))
                    parent bmemStates
      state       = Map.lookupDefault
                    (error ("PANIC (7): STATE NOT FOUND FOR " ++ show join))
                    join joinStates

  agendas <- forM (bmemToks parentState) $ \tok ->
    if performJoinTests (joinTests join) tok wme
      then leftActivateJoinChildren state tok wme
      else return A.empty

  return (A.concat agendas)
{-# INLINE rightActivateJoin #-}

leftActivateJoin :: Tok -> Join -> ReteM Agenda
leftActivateJoin tok join = do
  rete <- get
  let amemStates = reteAmemStates rete
      joinStates = reteJoinStates rete

      amem      = joinAmem join
      amemState = Map.lookupDefault
                  (error ("PANIC (8): STATE NOT FOUND FOR " ++ show amem))
                  amem amemStates
      state     = Map.lookupDefault
                  (error ("PANIC (9): STATE NOT FOUND FOR " ++ show join))
                  join joinStates

  if noJoinChildren state
    then return A.empty
    else do
      let wmes = matchingAmemWmes (joinTests join) tok amemState
      agendas <- forM wmes $ \wme ->
        leftActivateJoinChildren state tok wme

      return (A.concat agendas)
{-# INLINE leftActivateJoin #-}

leftActivateJoinChildren :: JoinState -> Tok -> Wme -> ReteM Agenda
leftActivateJoinChildren state tok wme = do
  agenda <- case joinChildBmem state of
    Just bmem -> leftActivateBmem bmem tok wme
    Nothing   -> return A.empty

  agendas <- mapM (leftActivateProd tok wme) (joinChildProds state)
  return (A.concat (agenda : agendas))
{-# INLINE leftActivateJoinChildren #-}

noJoinChildren :: JoinState -> Bool
noJoinChildren JoinState { joinChildBmem = bmem, joinChildProds = prods } =
  isNothing bmem && null prods
{-# INLINE noJoinChildren #-}

-- PROD

leftActivateProd :: Tok -> Wme -> Prod -> ReteM Agenda
leftActivateProd tok wme Prod { prodPreds    = preds
                              , prodAction   = action
                              , prodBindings = bindings }  = do
  let newTok     = wme:tok
      matching p = p bindings newTok

  if all matching preds
    then return (action bindings newTok)
    else return A.empty
{-# INLINE leftActivateProd #-}

-- ADDING WMES

-- | Adds a new fact represented by three fields.
addWme :: (ToConstant o, ToConstant a, ToConstant v) => o -> a -> v
       -> ReteM Agenda
addWme o a v = do
  (o', a', v')  <- internFields o a v
  workingMemory <- liftM reteWorkingMemory get
  let wme = Wme o' a' v'
  if Set.member wme (reteWmes workingMemory)
    then return A.empty -- Already present, do nothing.
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
  {-# INLINE toConstant   #-}

instance ToConstant Primitive where
  -- Every Primitive is treated as a Const.
  toConstant = return . PrimitiveConstant
  {-# INLINE toConstant #-}

instance ToConstant Bool where
  toConstant = toConstant . BoolPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Char where
  toConstant = toConstant . CharPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Double where
  toConstant = toConstant . DoublePrimitive
  {-# INLINE toConstant #-}

instance ToConstant Float where
  toConstant = toConstant . FloatPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Int where
  toConstant = toConstant . IntPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Int8 where
  toConstant = toConstant . Int8Primitive
  {-# INLINE toConstant #-}

instance ToConstant Int16 where
  toConstant = toConstant . Int16Primitive
  {-# INLINE toConstant #-}

instance ToConstant Int32 where
  toConstant = toConstant . Int32Primitive
  {-# INLINE toConstant #-}

instance ToConstant Int64 where
  toConstant = toConstant . Int64Primitive
  {-# INLINE toConstant #-}

instance ToConstant Integer where
  toConstant = toConstant . IntegerPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Word where
  toConstant = toConstant . WordPrimitive
  {-# INLINE toConstant #-}

instance ToConstant Word8 where
  toConstant = toConstant . Word8Primitive
  {-# INLINE toConstant #-}

instance ToConstant Word16 where
  toConstant = toConstant . Word16Primitive
  {-# INLINE toConstant #-}

instance ToConstant Word32 where
  toConstant = toConstant . Word32Primitive
  {-# INLINE toConstant #-}

instance ToConstant Word64  where
  toConstant = toConstant . Word64Primitive
  {-# INLINE toConstant #-}

instance ToConstant String where
  -- Raw String is always a constant.
  toConstant ""   = return emptyConstant
  toConstant name = internConstant name
  {-# INLINE toConstant   #-}

instance ToConstant NamedPrimitive where
  toConstant = return . NamedPrimitiveConstant
  {-# INLINE toConstant #-}

-- EXPLICIT CONSTRUCTORS FOR VARIABLES

type Var = ReteM Variable

-- | A type of values with a variable semantics.
class ToVar a where
  -- | Marks a thing as a variable resulting in a Symbolic value.
  var :: a -> Var

instance ToVar String where
  var "" = error "ERROR (1): EMPTY VARIABLE NAME."
  var s  = internVariable s
  {-# INLINE var #-}

instance ToVar NamedPrimitive where
  var (NamedPrimitive _ "") = error "ERROR (2): EMPTY VARIABLE NAME."
  var np                    = return (NamedPrimitiveVariable np)
  {-# INLINE var #-}

instance ToVar Var where
  var = id
  {-# INLINE var #-}

class ToConstantOrVariable a where
  toConstantOrVariable :: a -> ReteM ConstantOrVariable

instance ToConstantOrVariable Var where
  toConstantOrVariable = liftM JustVariable
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Primitive where
  -- Every Primitive is treated as a Const.
  toConstantOrVariable = return . JustConstant . PrimitiveConstant
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Bool where
  toConstantOrVariable = toConstantOrVariable . BoolPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Char where
  toConstantOrVariable = toConstantOrVariable . CharPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Double where
  toConstantOrVariable = toConstantOrVariable . DoublePrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Float where
  toConstantOrVariable = toConstantOrVariable . FloatPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Int where
  toConstantOrVariable = toConstantOrVariable . IntPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Int8 where
  toConstantOrVariable = toConstantOrVariable . Int8Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Int16 where
  toConstantOrVariable = toConstantOrVariable . Int16Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Int32 where
  toConstantOrVariable = toConstantOrVariable . Int32Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Int64 where
  toConstantOrVariable = toConstantOrVariable . Int64Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Integer where
  toConstantOrVariable = toConstantOrVariable . IntegerPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Word where
  toConstantOrVariable = toConstantOrVariable . WordPrimitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Word8 where
  toConstantOrVariable = toConstantOrVariable . Word8Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Word16 where
  toConstantOrVariable = toConstantOrVariable . Word16Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Word32 where
  toConstantOrVariable = toConstantOrVariable . Word32Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable Word64  where
  toConstantOrVariable = toConstantOrVariable . Word64Primitive
  {-# INLINE toConstantOrVariable #-}

instance ToConstantOrVariable String where
  -- Raw String is always a constant.
  toConstantOrVariable "" = return (JustConstant emptyConstant)
  toConstantOrVariable s  = liftM JustConstant (internConstant s)
  {-# INLINE toConstantOrVariable   #-}

instance ToConstantOrVariable NamedPrimitive where
  toConstantOrVariable = return . JustConstant . NamedPrimitiveConstant
  {-# INLINE toConstantOrVariable #-}
