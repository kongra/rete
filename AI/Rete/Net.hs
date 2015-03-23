{-# LANGUAGE Trustworthy          #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Net
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-05
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Net
    (
      -- * Adding productions
      addProd
    , addProdP

      -- * Creating conditions
    , c

      -- * Variable value access
    , val
    , valE
    , valM

      -- * Strategies
    , StepStrategy
    , breadthFirst
    , depthFirst

      -- * Forward chaining
    , forwardChain
    , exec
    , execIO

      -- * Predefined actions and tools
    , acompose
    , passAction
    , traceAction
    , traceMsgAction
    )
    where

import           AI.Rete.Data
import           AI.Rete.Flow
import           AI.Rete.State
import           Control.Monad (liftM, liftM3, forM, forM_, when)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust)
import           Debug.Trace
import           Kask.Control.Lens
import           Kask.Data.List (nthDef, takeWhileI)
import           Safe (headMay)

-- CREATING ALPHA MEMORY AND FEEDING IN INITIAL Wmes.

-- | Searches for an existing alpha memory for the given symbols or
-- creates a new one.
buildOrShareAmem :: Obj   ConstantOrVariable
                 -> Attr  ConstantOrVariable
                 -> Val   ConstantOrVariable
                 -> ReteM Amem
buildOrShareAmem (Obj o) (Attr a) (Val v) = do
  let f s = case s of { JustConstant c' -> c'; _ -> wildcardConstant }
      o'  = Obj  (f o)
      a'  = Attr (f a)
      v'  = Val  (f v)
      k   = Wme o' a' v'

  amems <- liftM (view reteAmems) (viewS Rete)

  case Map.lookup k amems of
    Just amem -> return amem  -- Happily found.
    Nothing   -> do
      -- Let's create new Amem.
      i <- genid
      let amem = Amem i o' a' v'
      state <- viewS Rete
      let amemState = createAmemState state o' a' v'

      overS ( over reteAmems      (Map.insert k    amem)
            . over reteAmemStates (Map.insert amem amemState)) Rete

      return amem

createAmemState :: ReteState
                -> Obj Constant -> Attr Constant -> Val Constant
                -> AmemState
createAmemState state o a v = loop wmes Map.empty Map.empty Map.empty
  where
    (Obj  o') = o
    (Attr a') = a
    (Val  v') = v
    isWild  s = s == wildcardConstant
    wmes      = Set.toList (wmesForAmemFeed (isWild o') (isWild a') (isWild v')
                            state o a v)

    loop []        i1 i2 i3 = AmemState wmes i1 i2 i3 []
    loop (w:wmes') i1 i2 i3 = let (Wme wo wa wv) = w in
      loop wmes' (wmesIndexInsert wo w i1)
                 (wmesIndexInsert wa w i2)
                 (wmesIndexInsert wv w i3)

wmesForAmemFeed :: Bool -> Bool -> Bool
                -> ReteState
                -> Obj Constant -> Attr Constant -> Val Constant
                -> Set.HashSet Wme
wmesForAmemFeed False False False state o a v =
  -- o a v
  s1 `Set.intersection` s2 `Set.intersection` s3
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj  state)
    s2 = Map.lookupDefault Set.empty a (view reteWmesByAttr state)
    s3 = Map.lookupDefault Set.empty v (view reteWmesByVal  state)

wmesForAmemFeed False False True state o a _ =
  -- o a [*]
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj  state)
    s2 = Map.lookupDefault Set.empty a (view reteWmesByAttr state)

wmesForAmemFeed False True False state o _ v =
  -- o [*] v
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj state)
    s2 = Map.lookupDefault Set.empty v (view reteWmesByVal state)

wmesForAmemFeed False True True state o _ _ =
  -- o [*] [*]
  Map.lookupDefault Set.empty o (view reteWmesByObj state)

wmesForAmemFeed True False False state _ a v =
  -- [*] a v
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty a (view reteWmesByAttr state)
    s2 = Map.lookupDefault Set.empty v (view reteWmesByVal  state)

wmesForAmemFeed True False True state _ a _ =
  -- [*] a [*]
  Map.lookupDefault Set.empty a (view reteWmesByAttr state)

wmesForAmemFeed True True False state _ _ v =
  -- [*] [*] v
  Map.lookupDefault Set.empty v (view reteWmesByVal state)

wmesForAmemFeed True True True state _ _ _ =
  -- [*] [*] [*]
  view reteWmes state

-- BETA MEMORY CREATION

buildOrShareBmem :: Join -> ReteM Bmem
buildOrShareBmem parent = do
  -- Search for existing Bmem to share.
  parentState <- viewS parent
  case view joinChildBmem parentState of
    Just bmem -> return bmem  -- Happily found.
    Nothing   -> do
      -- Let's create a new Bmem and bind it to its state.
      bmem <- liftM Bmem genid
      overS (over reteBmemStates (Map.insert bmem (BmemState [] []))) Rete

      -- Update with matches from above:
      -- 1. Set bmem as a single child of parent (join).
      setS parent $ JoinState (Just bmem) []

      -- 2. Right-activate parent
      amemState <- viewS (joinAmem parent)
      forM_ (view amemWmes amemState) $ \wme -> rightActivateJoin wme parent

      -- 3. Restore parent state with bmem inside.
      setS parent $ JoinState (Just bmem) (view joinChildProds parentState)
      return bmem

-- JOIN TESTS

type IndexedCond = (Int, Cond)

indexedConds :: [Cond] -> [IndexedCond]
indexedConds = zip [0 ..]
{-# INLINE indexedConds #-}


fieldEqualTo :: Cond -> ConstantOrVariable -> Maybe Field
fieldEqualTo (Cond (Obj o) (Attr a) (Val v)) s
  | o == s    = Just O
  | a == s    = Just A
  | v == s    = Just V
  | otherwise = Nothing
{-# INLINE fieldEqualTo #-}

matchingLocation :: ConstantOrVariable -> IndexedCond -> Maybe Location
matchingLocation s (i, cond) = case fieldEqualTo cond s of
  Nothing -> Nothing
  Just f  -> Just (Location i f)
{-# INLINE matchingLocation #-}

joinTestForField :: Int -> ConstantOrVariable -> Field -> [IndexedCond]
                 -> Maybe JoinTest
joinTestForField i v field earlierConds =
  case v of
    JustVariable _ -> case headMay (matches earlierConds) of
      Nothing              -> Nothing
      Just (Location i' f) -> Just (JoinTest field f (i - i' - 1))

    JustConstant _ -> Nothing -- No tests from Consts (non-Vars).
  where
    matches = map fromJust . filter isJust . map (matchingLocation v)

joinTestsForCondImpl :: Int
                     -> Obj  ConstantOrVariable
                     -> Attr ConstantOrVariable
                     -> Val  ConstantOrVariable
                     -> [IndexedCond] -> [JoinTest]
joinTestsForCondImpl i (Obj o) (Attr a) (Val v) earlierConds =
  result3
  where
    test1   = joinTestForField i o O earlierConds
    test2   = joinTestForField i a A earlierConds
    test3   = joinTestForField i v V earlierConds

    result1 = [fromJust test3 | isJust test3]
    result2 = if isJust test2 then fromJust test2 : result1 else result1
    result3 = if isJust test1 then fromJust test1 : result2 else result2

joinTestsForCond :: IndexedCond -> [IndexedCond] -> [JoinTest]
joinTestsForCond (i, Cond o a v) = joinTestsForCondImpl i o a v
{-# INLINE joinTestsForCond #-}

-- JOIN CREATION

buildOrShareJoin :: Bmem -> Amem -> [JoinTest] -> ReteM Join
buildOrShareJoin parent amem tests = do
  -- Search for an existing Join to share.
  parentState <- viewS parent
  let isMatchingJoin j = amem == joinAmem j && tests == joinTests j
  case headMay (filter isMatchingJoin (view bmemChildren parentState)) of
    Just join -> return join  -- Happily found.
    Nothing   -> do
      -- Let's create a new Join and bind it to its state.
      i <- genid
      let join = Join i tests amem parent
      overS (over reteJoinStates (Map.insert join (JoinState Nothing []))) Rete

      -- Register join as its parent's child.
      setS parent (over bmemChildren (join:) parentState)

      -- Add join to amem's successors.
      overS (over amemSuccessors (join:)) amem

      return join

-- CREATING CONDITIONS (USER SIDE)

-- | Condition (positive).
data C = C !(ReteM (Obj  ConstantOrVariable))
           !(ReteM (Attr ConstantOrVariable))
           !(ReteM (Val  ConstantOrVariable))

toField :: ToConstantOrVariable a => (ConstantOrVariable -> b) -> a -> ReteM b
toField f = liftM f . toConstantOrVariable
{-# INLINE toField #-}

-- | Creates a positive condition.
c :: (ToConstantOrVariable o, ToConstantOrVariable a, ToConstantOrVariable v)
  => o -> a -> v -> C
c o a v = C (toField Obj o) (toField Attr a) (toField Val v)

toCond :: C -> ReteM Cond
toCond (C o a v) = liftM3 Cond o a v
{-# INLINE toCond #-}

-- CONFIGURING AND ACCESSING VARIABLE BINDINGS (IN ACTIONS)

bindingsForConds :: Int -> [IndexedCond] -> Bindings
bindingsForConds tokLen = loop Map.empty
  where
    loop result []                                        = result
    loop result ((i, Cond (Obj o) (Attr a) (Val v)) : cs) =
      loop result3 cs
      where
        result1 = bindingsForCond o O d result
        result2 = bindingsForCond a A d result1
        result3 = bindingsForCond v V d result2
        d       = tokLen - i - 1

bindingsForCond :: ConstantOrVariable -> Field -> Int -> Bindings -> Bindings
bindingsForCond s f d result = case s of
  -- For constants leave the resulting bindings untouched.
  JustConstant _ -> result
  -- For vars avoid overriding existing bindings.
  JustVariable v -> if   Map.member v result    then result
                    else Map.insert v (Location d f) result

-- | A value of a variable inside an action.
data VarVal = ValidVarVal   !Constant
            | NoVarVal      !Variable

instance Show VarVal where
  show (ValidVarVal s ) = show s
  show (NoVarVal    v ) = "ERROR (3): NO VALUE FOR VAR " ++ show v  ++ "."

-- | Returns a value of a variable inside an Action.
val :: ToVar v => v -> Actx -> ReteM VarVal
val v Actx { actxProd = prod, actxTok = tok } = do
  v' <- var v
  case Map.lookup v' (prodBindings prod) of
    Nothing             -> return (NoVarVal v')
    Just (Location d f) -> return (ValidVarVal (fieldConstant f wme))
      where
        wme = nthDef (error ("PANIC (5): ILLEGAL INDEX " ++ show d)) d tok

-- | Works like val, but raises an early error when a valid value
-- can't be returned.
valE :: ToVar v => v -> Actx -> ReteM Constant
valE v actx = do
  result <- val v actx
  case result of { ValidVarVal c' -> return c'; _ -> error (show result) }

-- | Works like valE, but returns Nothing instead of raising an error.
valM :: ToVar v => v -> Actx -> ReteM (Maybe Constant)
valM v actx = do
  result <- val v actx
  case result of { ValidVarVal c' -> return (Just c'); _ -> return Nothing }

-- ADDING PRODUCTIONS

-- | Creates a task with default priority (0) that represents adding a
-- (Prod)uction.
addProd :: [C] -> [Pred] -> Action -> Task
addProd conds preds action = addProdP conds preds action 0

-- | Creates a task with given priority that represents adding a (Prod)uction.
addProdP :: [C] -> [Pred] -> Action -> Int -> Task
addProdP conds preds action priority =
  Task (addProdA conds preds action) priority Nothing

-- | Creates the Agenda in Rete monad that represents adding a (Prod)uction.
addProdA :: [C] -> [Pred] -> Action -> ReteM Agenda
addProdA cs preds action = do
  when (null cs)
    (error "ERROR (4): PRODUCTION MUST HAVE AT LEAST 1 CONDITION (0 GIVEN).")

  conds <- mapM toCond cs

  -- Build or share dummy (top-level) Join with no tests for first Cond.
  let Cond o a v : _ = conds
  dummyAmem <- buildOrShareAmem o a v
  dummyJoin <- buildOrShareJoin dtn dummyAmem []

  -- Build or share joins for the rest of conditions (if any).
  let ics      = indexedConds conds
      ic1:ics' = ics
  parent      <- buildOrShareJoins dummyJoin ics' [ic1]
  parentState <- viewS parent
  let parentBmem  = view joinChildBmem  parentState
      parentProds = view joinChildProds parentState

  -- Prepare bindings.
  let bindings = bindingsForConds (length ics) (reverse ics)

  -- Create Prod.
  i <- genid
  let prod = Prod { prodId       = i
                  , prodPreds    = preds
                  , prodAction   = action
                  , prodBindings = bindings }

  -- Update prod with matches from above.
  -- 1. Set prod as a single child of parent (last join).
  setS parent $ JoinState Nothing [prod]

  -- 2. Right-activate parent
  amemState <- viewS (joinAmem parent)
  agendas <- forM (view amemWmes amemState) $ \wme ->
    rightActivateJoin wme parent

  -- 3. Restore parent state with prod inside.
  setS parent $ JoinState parentBmem (prod:parentProds)

  return (concat agendas)

buildOrShareJoins :: Join -> [IndexedCond] -> [IndexedCond] -> ReteM Join
buildOrShareJoins higherJoin []       _            = return higherJoin
buildOrShareJoins higherJoin (ic:ics) earlierConds = do
  let (_, Cond o a v) = ic
      tests           = joinTestsForCond ic earlierConds
  amem   <- buildOrShareAmem o a v
  parent <- buildOrShareBmem higherJoin
  join   <- buildOrShareJoin parent amem tests

  buildOrShareJoins join ics (ic:earlierConds)

-- FORWARD CHAINING

-- | Evaluation strategy for Agendas.
type StepStrategy = Agenda  -- ^ Current Agenda.
                 -> Agenda  -- ^ Subsequent (child) Agenda.
                 -> Agenda  -- ^ Updated Agenda.

-- | Breadth-first strategy.
breadthFirst :: StepStrategy
breadthFirst = (++)

-- | Depth-first strategy.
depthFirst :: StepStrategy
depthFirst = flip (++)

forwardStep :: StepStrategy -> (Agenda, ReteState) -> (Agenda, ReteState)
forwardStep strategy (agenda, state) = run state newAgenda
  where
    newAgenda = case agenda of
      []     -> return []
      (t:ts) -> liftM (strategy ts) (taskValue t)

-- | Creates a forward chain of changes from the initial Agenda and
-- state to the empty agenda and target state.
forwardChain :: StepStrategy -> Agenda -> ReteState -> [(Agenda, ReteState)]
forwardChain strategy agenda state =
  takeWhileI haveSomeWork (iterate (forwardStep strategy) (agenda, state))
  where
    haveSomeWork = not . null . fst

-- | Returns the target state after forward chaining from the initial
-- agenda and state.
exec :: StepStrategy -> Agenda -> ReteState -> ReteState
exec strategy agenda = snd . last . forwardChain strategy agenda

-- | Works like exec but seqs the result before returning it in IO
-- monad.
execIO :: StepStrategy -> Agenda -> ReteState -> IO ReteState
execIO strategy agenda state = do
  let targetState = exec strategy agenda state
  seq targetState (return targetState)

-- SOME PREDEFINED ACTIONS AND RELATED UTILITIES

-- | Composes the passed Actions.
acompose :: [Action] -> Action
acompose as actx = concatMap passActx as
  where
    passActx action = action actx

-- | An action that doesn't do anything.
passAction :: Action
passAction _ = []

-- | Action that traces a text on execution. The text is generated
-- by the passed function f.
traceAction :: (Actx -> ReteM String) -> Action
traceAction f actx =
  [Task { taskValue    = f actx >>= traceM >> return []
        , taskPriority = 0
        , taskProd     = Just (actxProd actx) }]

-- | Action that traces a predefined message.
traceMsgAction :: String -> Action
traceMsgAction = traceAction . const . return
