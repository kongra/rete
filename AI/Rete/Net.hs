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
module AI.Rete.Net where

import           AI.Rete.Data
import           AI.Rete.Flow
import           AI.Rete.State
import           Control.Monad (liftM, forM_)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Maybe (isJust, fromJust)
import           Kask.Control.Lens
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

  amems <- liftM (view reteAmems) (viewS ())

  case Map.lookup k amems of
    Just amem -> return amem -- Happily found.
    Nothing   -> do
      -- Let's create new Amem.
      amem <- liftM Amem genid
      rete <- viewS ()
      let amemState = createAmemState rete o' a' v'

      overS ( over reteAmems      (Map.insert k    amem)
            . over reteAmemStates (Map.insert amem amemState)) ()

      return amem
{-# INLINE buildOrShareAmem #-}

createAmemState :: Rete
                -> Obj Constant -> Attr Constant -> Val Constant
                -> AmemState
createAmemState rete o a v = loop wmes Map.empty Map.empty Map.empty
  where
    (Obj  o') = o
    (Attr a') = a
    (Val  v') = v
    isWild  s = s == wildcardConstant
    wmes      = Set.toList (wmesForAmemFeed (isWild o') (isWild a') (isWild v')
                                            rete o a v)

    loop []        i1 i2 i3 = AmemState wmes i1 i2 i3 []
    loop (w:wmes') i1 i2 i3 = let (Wme wo wa wv) = w in
      loop wmes' (wmesIndexInsert wo w i1)
                 (wmesIndexInsert wa w i2)
                 (wmesIndexInsert wv w i3)
{-# INLINE createAmemState #-}

wmesForAmemFeed :: Bool -> Bool -> Bool
                -> Rete
                -> Obj Constant -> Attr Constant -> Val Constant
                -> Set.HashSet Wme
wmesForAmemFeed False False False rete o a v =
  -- o a v
  s1 `Set.intersection` s2 `Set.intersection` s3
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj  rete)
    s2 = Map.lookupDefault Set.empty a (view reteWmesByAttr rete)
    s3 = Map.lookupDefault Set.empty v (view reteWmesByVal  rete)

wmesForAmemFeed False False True rete o a _ =
  -- o a *
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj  rete)
    s2 = Map.lookupDefault Set.empty a (view reteWmesByAttr rete)

wmesForAmemFeed False True False rete o _ v =
  -- o * v
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty o (view reteWmesByObj rete)
    s2 = Map.lookupDefault Set.empty v (view reteWmesByVal rete)

wmesForAmemFeed False True True rete o _ _ =
  -- o * *
  Map.lookupDefault Set.empty o (view reteWmesByObj rete)

wmesForAmemFeed True False False rete _ a v =
  -- * a v
  s1 `Set.intersection` s2
  where
    s1 = Map.lookupDefault Set.empty a (view reteWmesByAttr rete)
    s2 = Map.lookupDefault Set.empty v (view reteWmesByVal  rete)

wmesForAmemFeed True False True rete _ a _ =
  -- * a *
  Map.lookupDefault Set.empty a (view reteWmesByAttr rete)

wmesForAmemFeed True True False rete _ _ v =
  -- * * v
  Map.lookupDefault Set.empty v (view reteWmesByVal rete)

wmesForAmemFeed True True True rete _ _ _ =
  -- * * *
  view reteWmes rete
{-# INLINE wmesForAmemFeed #-}

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
      overS (over reteBmemStates (Map.insert bmem (BmemState [] []))) ()

      -- Update with matches from above:
      -- 1. Set bmem as a single child of parent (join).
      setS parent $ JoinState (Just bmem) []

      -- 2. Right-activate parent
      amemState <- viewS (joinAmem parent)
      forM_ (view amemWmes amemState) $ \wme -> rightActivateJoin wme parent

      -- 3. Restore parent state with bmem inside.
      setS parent $ JoinState (Just bmem) (view joinChildProds parentState)
      return bmem
{-# INLINE buildOrShareBmem #-}

-- -- JOIN TESTS

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
{-# INLINE joinTestForField #-}

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
{-# INLINE joinTestsForCondImpl #-}

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
      overS (over reteJoinStates (Map.insert join (JoinState Nothing []))) ()

      -- Register join as its parent's child.
      setS parent (over bmemChildren (join:) parentState)
      return join
{-# INLINE buildOrShareJoin #-}
