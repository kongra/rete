{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Print
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-11-14
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- Textual visualization of Rete network and data.
------------------------------------------------------------------------
module AI.Rete.Print
    (
      -- * Print methods
      toShowS
    , toString

      -- * The 'Depth' constraints of the tree traversal process
    , Depth
    , depth
    , boundless

      -- * 'Switch'es
    , Switch
    , with, no, clear

      -- * Predefined compound 'Switch'es
    , soleNetTopDown
    , soleNetBottomUp

      -- * Predefined 'Switch'es
    , withNet
    , noNet
    , withData
    , noData
    , up
    , down
    , withIds
    , noIds

      -- * 'Flag's (detailed)
    , Flag (..)
    )
    where

import           AI.Rete.Algo
import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Monad (liftM)
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.List (intersperse)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Tree.Print
import           Kask.Control.Monad (toListM, mapMM)
import           Kask.Data.Function (compose, rcompose)

-- CONFIGURATION

-- | A Boolean (semanticaly) configuration option for the printing
-- process.
data Flag =
  -- Emph flag
  NetEmph | DataEmph

  -- Wme flags
  | WmeIds | WmeSymbolic | WmeAmems | WmeToks | WmeNegJoinResults

  -- Token flags
  | TokIds      | TokWmes        | TokWmesSymbolic | TokNodes | TokParents
  | TokChildren | TokJoinResults | TokNccResults   | TokOwners

  -- Amem flags
  | AmemFields | AmemRefCounts | AmemWmes | AmemSuccessors

  -- Node flags
  | NodeIds | NodeParents | NodeChildren

  -- Unlinking flags
  | Uls

  -- Bmem flags
  | BmemToks

  -- JoinNode flags
  | JoinTests  | JoinAmems | JoinNearestAncestors

  -- NegNode flags
  | NegTests   | NegAmems  | NegNearestAncestors | NegToks

  -- NccNode flags
  | NccPartners | NccToks

  -- NccPartner flags
  | NccNodes | NccNumberOfConjucts | NccNewResultBuffs

  -- PNode flags
  | PNodeBindings | PNodeToks deriving (Show, Eq)

flagCode :: Flag -> Int
flagCode NetEmph              = 0
flagCode DataEmph             = 1
flagCode WmeIds               = 2
flagCode WmeSymbolic          = 3
flagCode WmeAmems             = 4
flagCode WmeToks              = 5
flagCode WmeNegJoinResults    = 6
flagCode TokIds               = 7
flagCode TokWmes              = 8
flagCode TokWmesSymbolic      = 9
flagCode TokNodes             = 10
flagCode TokParents           = 11
flagCode TokChildren          = 12
flagCode TokJoinResults       = 13
flagCode TokNccResults        = 14
flagCode TokOwners            = 15
flagCode AmemFields           = 16
flagCode AmemRefCounts        = 17
flagCode AmemWmes             = 18
flagCode AmemSuccessors       = 19
flagCode NodeIds              = 20
flagCode NodeParents          = 21
flagCode NodeChildren         = 22
flagCode Uls                  = 23
flagCode BmemToks             = 24
flagCode JoinTests            = 25
flagCode JoinAmems            = 26
flagCode JoinNearestAncestors = 27
flagCode NegTests             = 28
flagCode NegAmems             = 29
flagCode NegNearestAncestors  = 30
flagCode NegToks              = 31
flagCode NccPartners          = 32
flagCode NccToks              = 33
flagCode NccNodes             = 34
flagCode NccNumberOfConjucts  = 35
flagCode NccNewResultBuffs    = 36
flagCode PNodeBindings        = 37
flagCode PNodeToks            = 38
{-# INLINE flagCode #-}

instance Hashable Flag where
  hashWithSalt salt f = salt `hashWithSalt` flagCode f

-- | A set of 'Flags's.
type Flags = Set.HashSet Flag

-- | A switch to turn the 'Flag's on/off.
type Switch = Flags -> Flags

-- | Creates a 'Switch' that turns the 'Flag' on.
with :: Flag -> Switch
with = Set.insert
{-# INLINE with #-}

-- | Creates a 'Switch' that turns the 'Flag' off.
no :: Flag -> Switch
no = Set.delete
{-# INLINE no #-}

-- | Creates a 'Switch' that turns all flags off.
clear :: Switch
clear _ = noFlags

-- | Asks whether the 'Flag' is on in 'Flags'.
is :: Flag -> Flags -> Bool
is = Set.member
{-# INLINE is #-}

-- | A set of 'Flag's with all 'Flag's turned off.
noFlags :: Flags
noFlags = Set.empty

-- PREDEFINED Switch CONFIGURATIONS

dataFlags :: [Flag]
dataFlags =  [ WmeToks
             , WmeNegJoinResults
             , TokWmes
             , TokParents
             , TokChildren
             , TokJoinResults
             , TokNccResults
             , TokOwners
             , AmemWmes
             , BmemToks
             , NegToks
             , NccToks
             , NccNewResultBuffs
             , NccNumberOfConjucts
             , PNodeToks ]

netFlags :: [Flag]
netFlags =  [ WmeAmems
            , TokNodes
            , AmemRefCounts
            , AmemSuccessors
            , NodeParents
            , NodeChildren
            , JoinTests
            , JoinAmems
            , JoinNearestAncestors
            , NegTests
            , NegAmems
            , NegNearestAncestors
            , NccPartners
            , NccNodes
            , PNodeBindings ]

idFlags :: [Flag]
idFlags =  [WmeIds, TokIds, NodeIds]

-- | A 'Switch' that turns data presentation off.
noData :: Switch
noData = compose (map no dataFlags)

-- | A 'Switch' that turns data presentation on.
withData :: Switch
withData = compose (map with dataFlags)

-- | A 'Switch' that turns network presentation off.
noNet :: Switch
noNet = compose (map no netFlags)

-- | A 'Switch' that turns network presentation on.
withNet :: Switch
withNet = compose (map with netFlags)

-- | A 'Switch' that imposes the presentation traversal from lower
-- nodes to higher.
up :: Switch
up = with NodeParents . no NodeChildren . no AmemSuccessors

-- | A 'Switch' that imposes the presentation traversal from higher
-- nodes to lower.
down :: Switch
down = with NodeChildren . no AmemSuccessors . no NodeParents

-- | A 'Switch' that turns IDs presentation off.
noIds :: Switch
noIds = compose (map no idFlags)

-- | A 'Switch' that turns Ids presentation on.
withIds :: Switch
withIds = compose (map with idFlags)

-- DEFENDING AGAINST CYCLES

data Visited = Visited { visitedWmes  :: !(Set.HashSet Wme)
                       , visitedToks  :: !(Set.HashSet Tok)
                       , visitedAmems :: !(Set.HashSet Amem)
                       , visitedNodes :: !(Set.HashSet Node) }

cleanVisited :: Visited
cleanVisited = Visited { visitedWmes  = Set.empty
                       , visitedToks  = Set.empty
                       , visitedAmems = Set.empty
                       , visitedNodes = Set.empty }

class Visitable a where
  visiting :: a -> Visited -> Visited
  visited  :: a -> Visited -> Bool

instance Visitable Wme where
  visiting wme vs = vs { visitedWmes = Set.insert wme (visitedWmes vs) }
  visited  wme vs = Set.member wme (visitedWmes vs)

instance Visitable Tok where
  visiting tok vs = vs { visitedToks = Set.insert tok (visitedToks vs) }
  visited  tok vs = Set.member tok (visitedToks vs)

instance Visitable Amem where
  visiting amem vs = vs { visitedAmems = Set.insert amem (visitedAmems vs) }
  visited  amem vs = Set.member amem (visitedAmems vs)

instance Visitable Node where
  visiting node vs = vs { visitedNodes = Set.insert node (visitedNodes vs) }
  visited  node vs = Set.member node (visitedNodes vs)

instance Visitable VLoc where
  visiting _ vs = vs -- no need to ever mark VLocs as visited
  visited  _ _  = False

instance Visitable JoinTest where
  visiting _ vs = vs -- no need to ever mark JoinTest as visited
  visited  _ _  = False

withEllipsis :: Bool -> ShowS -> STM ShowS
withEllipsis False s = return s
withEllipsis True  s = return (compose [s, showString " ..."])
{-# INLINE withEllipsis #-}

withEllipsisT :: Bool -> STM ShowS -> STM ShowS
withEllipsisT v s = s >>= withEllipsis v
{-# INLINE withEllipsisT #-}

whenNot :: Bool -> STM [Vn] -> STM [Vn]
whenNot True  _   = return []
whenNot False vns = vns
{-# INLINE whenNot #-}

-- Vns (VISUALIZATION NODEs)

type ShowVn = Flags -> Visited -> STM ShowS
type AdjsVn = Flags -> Visited -> STM [Vn]

class ToVn a where
  toShowVn :: a -> ShowVn
  toAdjsVn :: a -> AdjsVn

data Vn = Vn { vnShowM   :: !ShowVn
             , vnAdjs    :: !AdjsVn
             , vnVisited :: !Visited }

toVn :: ToVn a => Visited -> a -> Vn
toVn vs x = Vn { vnShowM   = toShowVn x
               , vnAdjs    = toAdjsVn x
               , vnVisited = vs }
{-# INLINE toVn #-}

instance ShowM STM Flags Vn where
  showM fs Vn { vnShowM = f, vnVisited = vs } = f fs vs

-- LEAF/PROPERTY Vns CREATION

leafVn :: Visited -> ShowVn -> Vn
leafVn vs svn = Vn { vnShowM   = svn
                   , vnAdjs    = \_ _ -> return []
                   , vnVisited = vs}
{-# INLINE leafVn #-}

propVn :: Visited -> ShowS -> [Vn] -> Vn
propVn vs name vns = Vn { vnShowM   = \_ _ -> return name
                        , vnAdjs    = \_ _ -> return vns
                        , vnVisited = vs}
{-# INLINE propVn #-}

leafPropVn :: Visited -> ShowS -> [ShowVn] -> Vn
leafPropVn vs name svns = propVn vs name (map (leafVn vs) svns)
{-# INLINE leafPropVn #-}

idS :: ID -> ShowS
idS id' = compose [showString " ", showString $ show id']
{-# INLINE idS #-}

withIdS :: ShowS -> ID -> ShowS
withIdS s id' = compose [s, idS id']
{-# INLINE withIdS #-}

withOptIdS :: Bool -> ShowS -> ID -> ShowS
withOptIdS False s _   = s
withOptIdS True  s id' = s `withIdS` id'
{-# INLINE withOptIdS #-}

toVnsM :: (Monad m, Foldable f, ToVn a) => Visited -> m (f a) -> m [Vn]
toVnsM vs = liftM (map (toVn vs)) . toListM
{-# INLINE toVnsM #-}

toShowVnsM :: (Monad m, Foldable f, ToVn a) => m (f a) -> m [ShowVn]
toShowVnsM = liftM (map toShowVn) . toListM
{-# INLINE toShowVnsM #-}

type OptPropVn = (Monad m, Foldable f, ToVn a) =>
                 Bool -> String -> Visited -> m (f a) -> m (Maybe Vn)

optPropVn :: OptPropVn
optPropVn False _     _  _  = return Nothing
optPropVn True  label vs xs = do
  vns <- toVnsM vs xs
  if null vns
     then return Nothing
     else return (Just (propVn vs (showString label) vns))
{-# INLINABLE optPropVn #-}

optLeafPropVn :: OptPropVn
optLeafPropVn False _     _  _  = return Nothing
optLeafPropVn True  label vs xs = do
  shows' <- toShowVnsM xs
  if null shows'
     then return Nothing
     else return (Just (leafPropVn vs (showString label) shows'))
{-# INLINABLE optLeafPropVn #-}

optVns :: Monad m => [Maybe Vn] -> m [Vn]
optVns = return . catMaybes
{-# INLINE optVns #-}

netPropVn :: Flags -> OptPropVn
netPropVn fs = if is NetEmph fs then optPropVn else optLeafPropVn
{-# INLINE netPropVn #-}

datPropVn :: Flags -> OptPropVn
datPropVn fs = if is DataEmph fs then optPropVn else optLeafPropVn
{-# INLINE datPropVn #-}

-- CONFIGURATION

type VConf = Conf STM ShowS Flags Vn

conf :: VConf
conf = Conf { impl     = stmImpl
            , adjs     = \fs Vn { vnAdjs = f, vnVisited = vs } -> f fs vs
            , maxDepth = Nothing
            , opts     = noFlags }

-- | A specifier of depth of the treePrint process.
type Depth = VConf -> VConf

-- | Sets the maxDepth of a configuration to the specified value.
depth :: Int -> Depth
depth d c = c { maxDepth = Just d }
{-# INLINE depth #-}

-- | Unlimits the maxDepth of a configuration.
boundless :: Depth
boundless c = c { maxDepth = Nothing }
{-# INLINE boundless #-}

applySwitch :: Switch -> VConf -> VConf
applySwitch switch c@Conf { opts = opts' } = c { opts = switch opts' }
{-# INLINE applySwitch #-}

-- STM IMPL

stmImpl :: Impl STM ShowS
stmImpl = str

-- WMES VISUALIZATION

instance ToVn Wme where
  toShowVn = showWme
  toAdjsVn = adjsWme

showWme :: Wme -> Flags -> Visited -> STM ShowS
showWme wme fs vs =
  withEllipsis (visited wme vs) $
    if is WmeSymbolic fs
      then showWmeSymbolic                wme
      else showWmeExplicit (is WmeIds fs) wme
{-# INLINE showWme #-}

showWmeSymbolic :: Wme -> ShowS
showWmeSymbolic wme = compose [showString "w", shows $ wmeId wme]
{-# INLINE showWmeSymbolic #-}

showWmeExplicit :: Bool -> Wme -> ShowS
showWmeExplicit oid
  Wme { wmeId = id', wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    withOptIdS oid
      (compose [ showString "("
               , shows obj,  showString ","
               , shows attr, showString ",", shows val
               , showString ")"])
      id'
{-# INLINE showWmeExplicit #-}

showWmeMaybe :: (Wme -> ShowS) -> Maybe Wme -> ShowS
showWmeMaybe _ Nothing    = showString "_"
showWmeMaybe f (Just wme) = f wme
{-# INLINE showWmeMaybe #-}

adjsWme :: Wme -> Flags -> Visited -> STM [Vn]
adjsWme
  wme@Wme { wmeAmems                = amems
          , wmeToks                 = toks
          , wmeNegJoinResults       = jresults} fs vs =
    whenNot (visited wme vs) $ do
      let vs' = visiting wme vs
      amemsVn <- netPropVn fs (is WmeAmems fs) "amems" vs' (readTVar amems)
      toksVn  <- datPropVn fs (is WmeToks  fs) "toks"  vs' (readTVar toks)
      njrsVn  <- datPropVn fs (is WmeNegJoinResults fs)
                 "neg. ⊳⊲ results (owners)" vs'
                 -- When visualizing the negative join results we only
                 -- show the owner tokens, cause wme in every negative join
                 -- result is this wme.
                 (mapMM (return . negativeJoinResultOwner) (toListT jresults))

      optVns [amemsVn, toksVn, njrsVn]
{-# INLINE adjsWme #-}

-- TOKENS VISUALIZATION

instance ToVn Tok where
  toAdjsVn = adjsTok
  toShowVn = showTok

showTok :: Tok -> Flags -> Visited -> STM ShowS
showTok tok@DummyTopTok {} fs vs =
  withEllipsis (visited tok vs) $
    withOptIdS (is TokIds fs) (showString "{}") (-1)

showTok tok fs vs = do
    let s = if is TokWmes fs
              then (if is TokWmesSymbolic fs
                      then showTokWmesSymbolic tok
                      else showTokWmesExplicit (is WmeIds fs) tok)
              else showString "{..}"
    withEllipsis (visited tok vs) $
      withOptIdS (is TokIds fs) s (tokId tok)
{-# INLINE showTok #-}

showTokWmesSymbolic :: Tok -> ShowS
showTokWmesSymbolic = showTokWmes showWmeSymbolic
{-# INLINE showTokWmesSymbolic #-}

showTokWmesExplicit :: Bool -> Tok -> ShowS
showTokWmesExplicit owmeids = showTokWmes (showWmeExplicit owmeids)
{-# INLINE showTokWmesExplicit #-}

showTokWmes :: (Wme -> ShowS) -> Tok -> ShowS
showTokWmes f = rcompose
              . intersperse (showString ",")
              . map (showWmeMaybe f)
              . tokWmes
{-# INLINE showTokWmes #-}

adjsTok :: Tok -> Flags -> Visited -> STM [Vn]
adjsTok tok@DummyTopTok { tokNode  = node
                        , tokChildren  = children } fs vs =
  whenNot (visited tok vs) $ do
    let vs' = visiting tok vs
    nodeVn     <- netPropVn fs (is TokNodes fs) "node" vs'
                  (return [node])
    childrenVn <- datPropVn fs (is TokChildren fs) "children" vs'
                  (readTVar children)
    optVns [nodeVn, childrenVn]

adjsTok
  tok@Tok { tokParent         = parent
          , tokOwner          = mowner
          , tokNode           = node
          , tokChildren       = children
          , tokNegJoinResults = jresults
          , tokNccResults     = nresults } fs vs =
    whenNot (visited tok vs) $ do
      let vs' = visiting tok vs
      nodeVn     <- netPropVn fs (is TokNodes   fs) "node"   vs' (return [node])
      parentVn   <- datPropVn fs (is TokParents fs) "parent" vs' (return [parent])
      ownerVn    <- datPropVn fs (is TokOwners  fs) "owner"  vs'
                    (liftM owner (readTVar mowner))
      childrenVn <- datPropVn fs (is TokChildren fs) "children" vs'
                      (readTVar children)
      jresultsVn <- datPropVn fs (is TokJoinResults fs) "neg. ⊳⊲ results (wmes)"
                      vs'
                      -- When visualizing the negative join results we only
                      -- show the wmes, cause owner in every negative join
                      -- result is this tok(en).
                      (mapMM (return . negativeJoinResultWme) (toListT jresults))
      nresultsVn <- datPropVn fs (is TokNccResults fs) "ncc results" vs'
                      (readTVar nresults)
      optVns [nodeVn, parentVn, ownerVn, childrenVn, jresultsVn, nresultsVn]
    where
      owner ow = case ow of
        Nothing -> []
        Just o  -> [o]
{-# INLINE adjsTok #-}

-- AMEMS VISUALIZATION

instance ToVn Amem where
  toAdjsVn = adjsAmem
  toShowVn = showAmem

showAmem :: Amem -> Flags -> Visited -> STM ShowS
showAmem
  amem@Amem { amemObj            = obj
            , amemAttr           = attr
            , amemVal            = val
            , amemReferenceCount = rcount } fs vs = do
    let alpha = showString "α"
    let repr  = if is AmemFields fs
                  then compose [alpha, showString " ("
                                , sS obj,  showString ","
                                , sS attr, showString ","
                                , sS val
                                , showString ")"]
                  else alpha
    withEllipsisT (visited amem vs) $
      if is AmemRefCounts fs
        then (do rc <- readTVar rcount
                 return $ compose [repr, showString " refcount ", shows rc])
        else return repr
  where
    sS s | s == wildcardSymbol = showString "*"
         | otherwise           = shows s
{-# INLINE showAmem #-}

adjsAmem :: Amem -> Flags -> Visited -> STM [Vn]
adjsAmem
  amem@Amem { amemSuccessors  = succs
            , amemWmes        = wmes } fs vs =
    whenNot (visited amem vs) $ do
      let vs' = visiting amem vs
      succVn <- netPropVn fs (is AmemSuccessors fs) "successors" vs'
                (readTVar succs)
      wmesVn <- datPropVn fs (is AmemWmes fs) "wmes" vs' (readTVar wmes)
      optVns [succVn, wmesVn]
{-# INLINE adjsAmem #-}

-- NODE VISUALIZATION

instance ToVn Node where
  toAdjsVn = adjsNode
  toShowVn = showNode

showNode :: Node -> Flags -> Visited -> STM ShowS
showNode node@DummyTopNode {} _ vs =
  withEllipsis (visited node vs) $ showString "DTN (β)"

showNode node fs vs = do
  let variant = nodeVariant node
  s <- case variant of
    Bmem       {} -> showBmem       variant fs
    JoinNode   {} -> showJoinNode   variant fs
    NegNode    {} -> showNegNode    variant fs
    NccNode    {} -> showNccNode    variant fs
    NccPartner {} -> showNccPartner variant fs
    PNode      {} -> showPNode      variant fs
    DTN        {} -> unreachableCode "showNode"

  withEllipsis (visited node vs) $
    withOptIdS (is NodeIds fs) s (nodeId node)
{-# INLINE showNode #-}

adjsNode :: Node -> Flags -> Visited -> STM [Vn]
adjsNode node@DummyTopNode { nodeVariant = variant } fs vs =
  whenNot (visited node vs) $ do
    let vs' = visiting node vs
    -- In the case of DTM, just like in any β memory, we traverse down
    -- using all children, also the unlinked ones.
    childrenVn <- netPropVn fs (is NodeChildren fs) "children (with all)" vs'
                  (bmemLikeChildren node)
                  -- (rvprop bmemAllChildren node)
    variantVns <- adjsDTN variant fs vs'
    optVns (variantVns ++ [childrenVn])

adjsNode
  node@Node { nodeParent    = parent
            , nodeChildren  = children
            , nodeVariant   = variant } fs vs =
    whenNot (visited node vs) $ do
      let vs' = visiting node vs
      parentVn <- netPropVn fs (is NodeParents  fs) "parent" vs'
                  (return [parent])

      -- In the case of β memory, we traverse down using all children,
      -- also the unlinked ones.
      childrenVn <- if isBmemLike variant
                      then netPropVn fs (is NodeChildren fs)
                           "children (with all)" vs' (bmemLikeChildren node)
                      else netPropVn fs (is NodeChildren fs) "children"
                           vs' (readTVar children)

      variantVns <- case variant of
        Bmem       {} -> adjsBmem       variant fs vs'
        JoinNode   {} -> adjsJoinNode   variant fs vs'
        NegNode    {} -> adjsNegNode    variant fs vs'
        NccNode    {} -> adjsNccNode    variant fs vs'
        NccPartner {} -> adjsNccPartner variant fs vs'
        PNode      {} -> adjsPNode      variant fs vs'
        DTN        {} -> unreachableCode "adjsNode"

      optVns (variantVns ++ [parentVn, childrenVn])
{-# INLINE adjsNode #-}

-- Bmem VISUALIZATION

showBmem :: NodeVariant -> Flags ->  STM ShowS
showBmem _ _ = return (showString "β")
{-# INLINE showBmem #-}

adjsBmem :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsBmem Bmem { nodeToks = toks } fs vs' = adjsBmemLike toks fs vs'
adjsBmem _                        _  _   = unreachableCode "adjsBmem"
{-# INLINE adjsBmem #-}

isBmemLike :: NodeVariant -> Bool
isBmemLike Bmem {} = True
isBmemLike DTN  {} = True
isBmemLike _       = False
{-# INLINE isBmemLike #-}

adjsBmemLike :: TSet Tok -> Flags -> Visited -> STM [Maybe Vn]
adjsBmemLike toks fs vs' = do
    toksVn <- datPropVn fs (is BmemToks fs) "toks" vs' (readTVar toks)
    return [toksVn]
{-# INLINE adjsBmemLike #-}

-- | In the case of Bmems and STM We merge nodeChildren and
-- bmemAllChildren.
bmemLikeChildren :: Node -> STM (Set.HashSet Node)
bmemLikeChildren node = do
  children    <- liftM Set.fromList (toListT (nodeChildren node))
  allChildren <- rvprop bmemAllChildren node
  return (children `Set.union` allChildren)
{-# INLINE bmemLikeChildren #-}

-- DTN VISUALIZATION

adjsDTN :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsDTN DTN { nodeToks = toks } fs vs' = adjsBmemLike toks fs vs'
adjsDTN _                       _  _   = unreachableCode "adjsDTN"
{-# INLINE adjsDTN #-}

-- JoinNode VISUALIZATION

showJoinNode :: NodeVariant -> Flags -> STM ShowS
showJoinNode JoinNode { leftUnlinked = lu, rightUnlinked = ru } fs =
    if is Uls fs
      then (do mark <- ulMark lu ru
               return (showString ('⊳':'⊲':' ':mark)))
      else return (showString "⊳⊲")

showJoinNode _ _ = unreachableCode "showJoinNode"
{-# INLINE showJoinNode #-}

ulSign :: Bool -> Char
ulSign True  = '-'  -- unlinked
ulSign False = '+'  -- linked
{-# INLINE ulSign #-}

ulMark :: TVar Bool -> TVar Bool -> STM String
ulMark lu ru = do
  l <- readTVar lu
  r <- readTVar ru
  return [ulSign l, '/', ulSign r]
{-# INLINE ulMark #-}

ulSingleMark :: TVar Bool -> STM String
ulSingleMark unl = do
  u <- readTVar unl
  return ['/', ulSign u]
{-# INLINE ulSingleMark #-}

adjsJoinNode :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsJoinNode
  JoinNode { joinTests                   = tests
           , nodeAmem                    = amem
           , nearestAncestorWithSameAmem = ancestor } fs vs' = do
    testsVn    <- netPropVn fs (is JoinTests fs) "tests" vs' (return tests)
    amemVn     <- netPropVn fs (is JoinAmems fs) "amem"  vs' (return [amem])
    ancestorVn <- netPropVn fs (is JoinNearestAncestors fs) "ancestor" vs'
                  (joinAncestorM ancestor)
    return [amemVn, ancestorVn, testsVn]

adjsJoinNode _ _ _ = unreachableCode "adjsJoinNode"
{-# INLINABLE adjsJoinNode #-}

joinAncestorM :: Monad m => Maybe a -> m [a]
joinAncestorM ancestor = case ancestor of
  Nothing -> return []
  Just a  -> return [a]
{-# INLINE joinAncestorM #-}

-- NegNode VISUALIZATION

showNegNode :: NodeVariant -> Flags -> STM ShowS
showNegNode
  NegNode { rightUnlinked = ru } fs =
    if is Uls fs
      then (do mark <- ulSingleMark ru
               return (showString ('¬':' ':mark)))
      else return (showString "¬")

showNegNode _ _ = unreachableCode "showNegNode"
{-# INLINE showNegNode #-}

adjsNegNode :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsNegNode
  NegNode { joinTests                   = tests
          , nodeAmem                    = amem
          , nearestAncestorWithSameAmem = ancestor
          , nodeToks                  = toks} fs vs' = do
    amemVn     <- netPropVn fs (is NegAmems fs) "amem"  vs' (return [amem])
    testsVn    <- netPropVn fs (is NegTests fs) "tests" vs' (return tests)
    ancestorVn <- netPropVn fs (is NegNearestAncestors fs) "ancestor" vs'
                    (joinAncestorM ancestor)
    toksVn     <- datPropVn fs (is NegToks  fs) "toks" vs' (readTVar toks)
    return [amemVn, ancestorVn, testsVn, toksVn]

adjsNegNode _ _ _ = unreachableCode "adjsNegNode"

-- NccNode VISUALIZATION

showNccNode :: NodeVariant -> Flags -> STM ShowS
showNccNode NccNode {} _ = return (showString "Ncc")
showNccNode _          _ = unreachableCode "showNccNode"
{-# INLINE showNccNode #-}

adjsNccNode :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsNccNode NccNode { nodeToks = toks, nccPartner = partner } fs vs' = do
    partnerVn <- netPropVn fs (is NccPartners fs) "partner" vs'
                 (return [partner])
    toksVn    <- datPropVn fs (is NccToks fs) "toks" vs'
                 (readTVar toks)
    return [partnerVn, toksVn]

adjsNccNode _ _ _ = unreachableCode "adjsNccNode"
{-# INLINE adjsNccNode #-}

-- NccPartner VISUALIZATION

showNccPartner :: NodeVariant -> Flags -> STM ShowS
showNccPartner NccPartner { nccPartnerNumberOfConjucts = conjs  } fs =
    if is NccNumberOfConjucts fs
      then return (compose [showString "Ncc (P) conjucts ", shows conjs])
      else return (showString "Ncc (P)")

showNccPartner _ _ = unreachableCode "showNccPartner"
{-# INLINE showNccPartner #-}

adjsNccPartner :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsNccPartner
  NccPartner { nccPartnerNccNode       = node
             , nccPartnerNewResultBuff = buff } fs vs' = do
    node'  <- readTVar node
    nodeVn <- netPropVn fs (is NccNodes fs)
                "ncc node" vs' (return [fromJust node'])
    toksVn <- datPropVn fs (is NccNewResultBuffs fs)
                "new result buff." vs' (readTVar buff)
    return [nodeVn, toksVn]

adjsNccPartner _ _ _ = unreachableCode "adjsNccPartner"
{-# INLINE adjsNccPartner #-}

-- PNode VISUALIZATION

showPNode :: NodeVariant -> Flags -> STM ShowS
showPNode PNode {} _ = return (showString "P")
showPNode _        _ = unreachableCode "showPNode"
{-# INLINE showPNode #-}

adjsPNode :: NodeVariant -> Flags -> Visited -> STM [Maybe Vn]
adjsPNode
  PNode { nodeToks              = toks
        , pnodeVariableBindings = bindings } fs vs' = do
    toksVn <- datPropVn fs (is PNodeToks     fs) "toks" vs' (readTVar toks)
    varsVn <- netPropVn fs (is PNodeBindings fs) "vars" vs' (varlocs bindings)
    return [varsVn, toksVn]

adjsPNode _ _ _ = unreachableCode "adjsPNode"
{-# INLINE adjsPNode #-}

-- VARIABLE LOCATIONS CREATION AND VISUALIZATION

data VLoc = VLoc !Symbol !Field !Distance

varlocs :: VariableBindings -> STM [VLoc]
varlocs = return . map vbinding2VLoc . Map.toList
  where vbinding2VLoc (s, SymbolLocation f d) = VLoc s f d
{-# INLINE varlocs #-}

instance ToVn VLoc where
  toAdjsVn = adjsVLoc
  toShowVn = showVLoc

showVLoc :: VLoc -> Flags -> Visited -> STM ShowS
showVLoc (VLoc s f d) _ _ =
  return (compose [ shows s, showString " → "
                  , shows d, showString ",", shows f])
{-# INLINE showVLoc #-}

adjsVLoc :: VLoc -> Flags -> Visited -> STM [Vn]
adjsVLoc _ _ _ = return []
{-# INLINE adjsVLoc #-}

-- JoinTest VISUALIZATION

instance ToVn JoinTest where
  toAdjsVn = adjsJoinTest
  toShowVn = showJoinTest

showJoinTest :: JoinTest -> Flags -> Visited -> STM ShowS
showJoinTest
  JoinTest { joinTestField1   = f1
           , joinTestField2   = f2
           , joinTestDistance = d } _ _ =
    return (compose [ showString "⟨"
                    , shows f1, showString ","
                    , shows d,  showString ","
                    , shows f2
                    , showString "⟩"])
{-# INLINE showJoinTest #-}

adjsJoinTest :: JoinTest -> Flags -> Visited -> STM [Vn]
adjsJoinTest _ _ _ = return []
{-# INLINE adjsJoinTest #-}

-- MISC.

unreachableCode :: String -> a
unreachableCode tag
  = error ("Unreachable code. Impossible has happened!!! " ++ tag)
{-# INLINE unreachableCode #-}

-- PRINT IMPLEMENTATION

-- | Converts the selected object to a tree representation (expressed
-- in ShowS).
toShowS :: ToVn a => Depth -> Switch -> a -> STM ShowS
toShowS d switch obj = printTree (switches conf) (toVn cleanVisited obj)
  where switches = d . applySwitch switch
{-# INLINE toShowS #-}

-- | Works like toShowS, but returns String instead of ShowS
toString :: ToVn a => Depth -> Switch -> a -> STM String
toString d switch = liftM evalShowS . toShowS d switch
  where evalShowS s = s ""
{-# INLINE toString #-}

-- PREDEFINED PRINT CONFIGURATIONS

-- | A 'Switch' for presenting sole Rete net bottom-up.
soleNetBottomUp :: Switch
soleNetBottomUp = up . with NetEmph . withNet . withIds . with AmemFields
                . with Uls

-- | A 'Switch' for presenting sole Rete net top-down.
soleNetTopDown :: Switch
soleNetTopDown = down . with NetEmph . withNet . withIds . with AmemFields
               . with Uls
