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

      -- * The constraints of the process
    , Depth
    , depth
    , boundless

      -- * Configuration
    , Mode (Net, Data)
    , Switch
    , Flags
    , with, no
    , defaultFlags
    , Flag (..)
    )
    where

import           AI.Rete.Algo
import           AI.Rete.Data
import           Control.Concurrent.STM
import           Control.Monad (liftM)
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as Map
import           Data.List (intersperse)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Tree.Print
import           Kask.Control.Monad (toListM, mapMM)
import           Kask.Data.Function (compose, rcompose)

import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)

-- CONFIGURATION

-- | A Boolean (semanticaly) configuration option for the printing
-- process.
data Flag =
  -- Mode flags
  DataMode

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
  | BmemToks  | BmemAllChildren

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
flagCode DataMode             = 1
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
flagCode BmemAllChildren      = 25
flagCode JoinTests            = 26
flagCode JoinAmems            = 27
flagCode JoinNearestAncestors = 28
flagCode NegTests             = 29
flagCode NegAmems             = 30
flagCode NegNearestAncestors  = 31
flagCode NegToks              = 32
flagCode NccPartners          = 33
flagCode NccToks              = 34
flagCode NccNodes             = 35
flagCode NccNumberOfConjucts  = 36
flagCode NccNewResultBuffs    = 37
flagCode PNodeBindings        = 38
flagCode PNodeToks            = 39
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

-- | Asks whether the 'Flag' is on in 'Flags'.
is :: Flag -> Flags -> Bool
is = Set.member
{-# INLINE is #-}

-- | A default set of 'Flag's with all 'Flag's turned off.
defaultFlags :: Flags
defaultFlags = Set.empty

-- Vns (VISUALIZATION NODEs)

class ToVn a where
  toAdjsVn :: a -> AdjsVn
  toShowVn :: a -> ShowVn

toVn :: ToVn a => a -> Vn
toVn x = Vn { vnShowM = toShowVn x , vnAdjs  = toAdjsVn x }
{-# INLINE toVn #-}

type ShowVn = Flags -> STM ShowS
type AdjsVn = Flags -> STM [Vn]

data Vn =
  Vn { vnShowM :: !ShowVn
     , vnAdjs  :: !AdjsVn}

instance ShowM STM Flags Vn where showM o Vn { vnShowM = f } = f o

-- LEAF/PROPERTY Vns CREATION

emptyAdjs :: Monad m => a -> m [t]
emptyAdjs = return . const []
{-# INLINE emptyAdjs #-}

leafVn :: ShowVn -> Vn
leafVn svn = Vn { vnShowM = svn
                , vnAdjs  = emptyAdjs }
{-# INLINE leafVn #-}

propVn :: ShowS -> [Vn] -> Vn
propVn name vns = Vn { vnShowM = \_ -> return name
                     , vnAdjs  = return . const vns }
{-# INLINE propVn #-}

leafPropVn :: ShowS -> [ShowVn] -> Vn
leafPropVn name svns = propVn name (map leafVn svns)
{- INLINE leafPropVn -}

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

toVnsM :: (Monad m, Foldable f, ToVn a) => m (f a) -> m [Vn]
toVnsM = liftM (map toVn) . toListM
{-# INLINE toVnsM #-}

toShowVnsM :: (Monad m, Foldable f, ToVn a) => m (f a) -> m [ShowVn]
toShowVnsM = liftM (map toShowVn) . toListM
{-# INLINE toShowVnsM #-}

type OptPropVn = (Monad m, Foldable f, ToVn a) =>
                 Bool -> String -> m (f a) -> m (Maybe Vn)

optPropVn :: OptPropVn
optPropVn False _    _  = return Nothing
optPropVn True label xs = do
  vns <- toVnsM xs
  if null vns
     then return Nothing
     else return (Just (propVn (showString label) vns))
{-# INLINABLE optPropVn #-}

optLeafPropVn :: OptPropVn
optLeafPropVn False _    _  = return Nothing
optLeafPropVn True label xs = do
  shows' <- toShowVnsM xs
  if null shows'
     then return Nothing
     else return (Just (leafPropVn (showString label) shows'))
{-# INLINABLE optLeafPropVn #-}

optVns :: Monad m => [Maybe Vn] -> m [Vn]
optVns = return . catMaybes
{-# INLINE optVns #-}

netPropVn :: Flags -> OptPropVn
netPropVn fs = if is DataMode fs then optLeafPropVn else optPropVn
{-# INLINE netPropVn #-}

datPropVn :: Flags -> OptPropVn
datPropVn fs = if is DataMode fs then optPropVn else optLeafPropVn
{-# INLINE datPropVn #-}

-- CONFIGURATION

type VConf = Conf STM ShowS Flags Vn

conf :: VConf
conf = Conf { impl     = stmImpl
            , adjs     = \o Vn { vnAdjs = f } -> f o
            , maxDepth = Nothing
            , opts     = defaultFlags }

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
applySwitch oswitch c@Conf { opts = opts' } = c { opts = oswitch opts' }
{-# INLINE applySwitch #-}

-- | A mode of operating on Rete objects.
data Mode = Net  -- ^ Puts an emphasis on the structure of the network.
          | Data -- ^ Puts an emphasis on the data stored insite the network.

modeSwitch :: Mode -> Switch
modeSwitch Net  = no   DataMode
modeSwitch Data = with DataMode
{-# INLINE modeSwitch #-}

-- STM IMPL

stmImpl :: Impl STM ShowS
stmImpl = str

-- WMES VISUALIZATION

instance ToVn Wme where
  toAdjsVn = adjsWme
  toShowVn = showWme

showWme :: Wme -> Flags -> STM ShowS
showWme wme fs =
  if is WmeSymbolic fs
    then return (showWmeSymbolic                wme)
    else return (showWmeExplicit (is WmeIds fs) wme)
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

adjsWme :: Wme -> Flags -> STM [Vn]
adjsWme
  Wme { wmeAmems                = amems
      , wmeToks                 = toks
      , wmeNegJoinResults       = jresults} fs = do

    amemVn <- netPropVn fs (is WmeAmems fs) "amems" (readTVar amems)
    toksVn <- datPropVn fs (is WmeToks  fs) "toks"  (readTVar toks)
    njrsVn <- datPropVn fs (is WmeNegJoinResults fs)
                "neg. ⊳⊲ results (owners)"
                -- When visualizing the negative join results we only
                -- show the owner tokens, cause wme in every negative join
                -- result is this wme.
                (mapMM (return . negativeJoinResultOwner) (toListT jresults))

    optVns [amemVn, toksVn, njrsVn]
{-# INLINE adjsWme #-}

-- TOKENS VISUALIZATION

instance ToVn Tok where
  toAdjsVn = adjsTok
  toShowVn = showTok

showTok :: Tok -> Flags -> STM ShowS
showTok DummyTopTok {} fs
  = return (withOptIdS (is TokIds fs) (showString "{}") (-1))

showTok tok fs = do
    let s = if is TokWmes fs
              then (if is TokWmesSymbolic fs
                      then showTokWmesSymbolic tok
                      else showTokWmesExplicit (is WmeIds fs) tok)
              else showString "{..}"
    return (withOptIdS (is TokIds fs) s (tokId tok))
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

adjsTok :: Tok -> Flags -> STM [Vn]
adjsTok DummyTopTok { tokNode  = node,  tokChildren  = children } fs = do
    nodeVn     <- netPropVn fs (is TokNodes    fs) "node"     (return [node])
    childrenVn <- datPropVn fs (is TokChildren fs) "children" (readTVar children)
    optVns [nodeVn, childrenVn]

adjsTok
  Tok { tokParent         = parent
      , tokOwner          = mowner
      , tokNode           = node
      , tokChildren       = children
      , tokNegJoinResults = jresults
      , tokNccResults     = nresults } fs = do

    parentVn <- datPropVn fs (is TokParents fs) "parent" (return [parent])
    ownerVn  <- datPropVn fs (is TokOwners  fs) "owner"
                  (liftM owner (readTVar mowner))
    nodeVn   <- netPropVn fs (is TokNodes fs) "node" (return [node])

    childrenVn <- datPropVn fs (is TokChildren fs) "children"
                    (readTVar children)
    jresultsVn <- datPropVn fs (is TokJoinResults fs) "neg. ⊳⊲ results (wmes)"
                    -- When visualizing the negative join results we only
                    -- show the wmes, cause owner in every negative join
                    -- result is this tok(en).
                    (mapMM (return . negativeJoinResultWme) (toListT jresults))
    nresultsVn <- datPropVn fs (is TokNccResults fs) "ncc results"
                    (readTVar nresults)

    optVns [parentVn, ownerVn, nodeVn, childrenVn, jresultsVn, nresultsVn]
    where
      owner ow = case ow of
        Nothing -> []
        Just o  -> [o]
{-# INLINE adjsTok #-}

-- AMEMS VISUALIZATION

instance ToVn Amem where
  toAdjsVn = adjsAmem
  toShowVn = showAmem

showAmem :: Amem -> Flags -> STM ShowS
showAmem
  Amem { amemObj            = obj
       , amemAttr           = attr
       , amemVal            = val
       , amemReferenceCount = rcount } fs = do
    let alpha = showString "α"
    let repr  = if is AmemFields fs
                  then compose [alpha, showString " ("
                                , sS obj,  showString ","
                                , sS attr, showString ","
                                , sS val
                                , showString ")"]
                  else alpha
    if is AmemRefCounts fs
      then (do rc <- readTVar rcount
               return $ compose [repr, showString " refcount ", shows rc])
      else return repr
  where
    sS s | s == wildcardSymbol = showString "*"
         | otherwise           = shows s
{-# INLINE showAmem #-}

adjsAmem :: Amem -> Flags -> STM [Vn]
adjsAmem
  Amem { amemSuccessors  = succs
       , amemWmes        = wmes } fs = do
    succVn <- netPropVn fs (is AmemSuccessors fs) "successors" (readTVar succs)
    wmesVn <- datPropVn fs (is AmemWmes       fs) "wmes"       (readTVar wmes)
    optVns [succVn, wmesVn]
{-# INLINE adjsAmem #-}

-- NODE VISUALIZATION

instance ToVn Node where
  toAdjsVn = adjsNode
  toShowVn = showNode

showNode :: Node -> Flags -> STM ShowS
showNode DummyTopNode {} _ = return (showString "DTN (β)")

showNode node fs = do
  let variant = nodeVariant node
  s <- case variant of
    Bmem       {} -> showBmem       variant fs
    JoinNode   {} -> showJoinNode   variant fs
    NegNode    {} -> showNegNode    variant fs
    NccNode    {} -> showNccNode    variant fs
    NccPartner {} -> showNccPartner variant fs
    PNode      {} -> showPNode      variant fs
    DTN        {} -> unreachableCode

  return (withOptIdS (is NodeIds fs) s (nodeId node))
{-# INLINE showNode #-}

adjsNode :: Node -> Flags -> STM [Vn]
adjsNode
  DummyTopNode { nodeChildren  = children
               , nodeVariant   = variant } fs = do
    childrenVn <- netPropVn fs (is NodeChildren fs) "children" (readTVar children)
    variantVns <- adjsDTN variant fs
    optVns (variantVns ++ [childrenVn])

adjsNode
  Node { nodeParent    = parent
       , nodeChildren  = children
       , nodeVariant   = variant } fs = do
    childrenVn <- netPropVn fs (is NodeChildren fs) "children" (readTVar children)
    parentVn   <- netPropVn fs (is NodeParents  fs) "parent"   (return [parent])
    variantVns <- case variant of
      Bmem       {} -> adjsBmem       variant fs
      JoinNode   {} -> adjsJoinNode   variant fs
      NegNode    {} -> adjsNegNode    variant fs
      NccNode    {} -> adjsNccNode    variant fs
      NccPartner {} -> adjsNccPartner variant fs
      PNode      {} -> adjsPNode      variant fs
      DTN        {} -> unreachableCode

    optVns (variantVns ++ [parentVn, childrenVn])
{-# INLINE adjsNode #-}

-- Bmem VISUALIZATION

showBmem :: NodeVariant -> Flags -> STM ShowS
showBmem _ _ = return (showString "β")
{-# INLINE showBmem #-}

adjsBmemLike :: TSet Tok -> TSet Node -> Flags -> STM [Maybe Vn]
adjsBmemLike toks achildren fs = do
    achildrenVn <- netPropVn fs (is BmemAllChildren fs) "all children"
                     (readTVar achildren)
    toksVn      <- datPropVn fs (is BmemToks fs) "toks" (readTVar toks)
    return [achildrenVn, toksVn]
{-# INLINE adjsBmemLike #-}

adjsBmem :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsBmem Bmem { nodeToks = toks,  bmemAllChildren  = achildren  } fs =
  adjsBmemLike toks achildren fs
adjsBmem _ _ = unreachableCode
{-# INLINE adjsBmem #-}

-- DTN VISUALIZATION

adjsDTN :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsDTN DTN { nodeToks = toks,  bmemAllChildren  = achildren  } fs =
  adjsBmemLike toks achildren fs
adjsDTN _ _ = unreachableCode
{-# INLINE adjsDTN #-}

-- JoinNode VISUALIZATION

showJoinNode :: NodeVariant -> Flags -> STM ShowS
showJoinNode JoinNode { leftUnlinked = lu, rightUnlinked = ru } fs =
    if is Uls fs
      then (do mark <- ulMark lu ru
               return (showString ('⊳':'⊲':' ':mark)))
      else return (showString "⊳⊲")

showJoinNode _ _ = unreachableCode
{-# INLINE showJoinNode #-}

ulSign :: Bool -> Char
ulSign True  = '-'
ulSign False = '+'
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
  return ['_', '/', ulSign u]
{-# INLINE ulSingleMark #-}

adjsJoinNode :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsJoinNode
  JoinNode { joinTests                   = tests
           , nodeAmem                    = amem
           , nearestAncestorWithSameAmem = ancestor } fs = do

    testsVn    <- netPropVn fs (is JoinTests fs) "tests" (return tests)
    amemVn     <- netPropVn fs (is JoinAmems fs) "amem"  (return [amem])
    ancestorVn <- netPropVn fs (is JoinNearestAncestors fs)
                    "ancestor" (joinAncestorM ancestor)
    return [amemVn, ancestorVn, testsVn]

adjsJoinNode _ _ = unreachableCode
{-# INLINABLE adjsJoinNode #-}

joinAncestorM :: Monad m => Maybe a -> m [a]
joinAncestorM ancestor = case ancestor of
  Nothing -> return []
  Just a  -> return [a]
{-# INLINE joinAncestorM #-}

-- NegNode VISUALIZATION

showNegNode :: NodeVariant -> Flags -> STM ShowS
showNegNode
  JoinNode { rightUnlinked = ru } fs =
    if is Uls fs
      then (do mark <- ulSingleMark ru
               return (showString ('¬':' ':mark)))
      else return (showString "¬")

showNegNode _ _ = unreachableCode
{-# INLINE showNegNode #-}

adjsNegNode :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsNegNode
  NegNode { joinTests                   = tests
          , nodeAmem                    = amem
          , nearestAncestorWithSameAmem = ancestor
          , nodeToks                  = toks} fs = do

    amemVn     <- netPropVn fs (is NegAmems fs) "amem"  (return [amem])
    testsVn    <- netPropVn fs (is NegTests fs) "tests" (return tests)
    ancestorVn <- netPropVn fs (is NegNearestAncestors fs) "ancestor"
                    (joinAncestorM ancestor)
    toksVn     <- datPropVn fs (is NegToks  fs) "toks" (readTVar toks)

    return [amemVn, ancestorVn, testsVn, toksVn]

adjsNegNode _ _ = unreachableCode

-- NccNode VISUALIZATION

showNccNode :: NodeVariant -> Flags -> STM ShowS
showNccNode NccNode {} _ = return (showString "Ncc")
showNccNode _          _ = unreachableCode
{-# INLINE showNccNode #-}

adjsNccNode :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsNccNode NccNode { nodeToks = toks, nccPartner = partner } fs = do
    partnerVn <- netPropVn fs (is NccPartners fs) "partner" (return [partner])
    toksVn    <- datPropVn fs (is NccToks fs)     "toks"    (readTVar toks)
    return [partnerVn, toksVn]

adjsNccNode _ _ = unreachableCode
{-# INLINE adjsNccNode #-}

-- NccPartner VISUALIZATION

showNccPartner :: NodeVariant -> Flags -> STM ShowS
showNccPartner NccPartner { nccPartnerNumberOfConjucts = conjs  } fs =
    if is NccNumberOfConjucts fs
      then return (compose [showString "Ncc (P) conjucts ", shows conjs])
      else return (showString "Ncc (P)")

showNccPartner _ _ = unreachableCode
{-# INLINE showNccPartner #-}

adjsNccPartner :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsNccPartner
  NccPartner { nccPartnerNccNode       = node
             , nccPartnerNewResultBuff = buff } fs = do
    node'  <- readTVar node
    nodeVn <- netPropVn fs (is NccNodes fs)
                "ncc node" (return [fromJust node'])
    toksVn <- datPropVn fs (is NccNewResultBuffs fs)
                "new result buff." (readTVar buff)
    return [nodeVn, toksVn]

adjsNccPartner _ _ = unreachableCode
{-# INLINE adjsNccPartner #-}

-- PNode VISUALIZATION

showPNode :: NodeVariant -> Flags -> STM ShowS
showPNode PNode {} _ = return (showString "P")
showPNode _        _ = unreachableCode
{-# INLINE showPNode #-}

adjsPNode :: NodeVariant -> Flags -> STM [Maybe Vn]
adjsPNode
  PNode { nodeToks              = toks
        , pnodeVariableBindings = bindings } fs = do
    toksVn <- datPropVn fs (is PNodeToks     fs) "toks" (readTVar toks)
    locsVn <- netPropVn fs (is PNodeBindings fs) "vars" (varlocs bindings)
    return [locsVn, toksVn]

adjsPNode _ _ = unreachableCode
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

showVLoc :: VLoc -> Flags -> STM ShowS
showVLoc (VLoc s f d) _ =
  return (compose [ shows s, showString " → "
                  , shows d, showString ",", shows f])
{-# INLINE showVLoc #-}

adjsVLoc :: VLoc -> Flags -> STM [Vn]
adjsVLoc _ _ = return []
{-# INLINE adjsVLoc #-}

-- JoinTest VISUALIZATION

instance ToVn JoinTest where
  toAdjsVn = adjsJoinTest
  toShowVn = showJoinTest

showJoinTest :: JoinTest -> Flags -> STM ShowS
showJoinTest
  JoinTest { joinTestField1   = f1
           , joinTestField2   = f2
           , joinTestDistance = d } _ =
    return (compose [ showString "⟨"
                    , shows f1, showString ","
                    , shows d,  showString ","
                    , shows f2
                    , showString "⟩"])
{-# INLINE showJoinTest #-}

adjsJoinTest :: JoinTest -> Flags -> STM [Vn]
adjsJoinTest _ _ = return []
{-# INLINE adjsJoinTest #-}

-- MISC.

unreachableCode :: a
unreachableCode = error "Unreachable code. Impossible has happened!!!"
{-# INLINE unreachableCode #-}

-- PRINT IMPLEMENTATION

-- | Converts the selected object to a tree representation (expressed
-- in ShowS).
toShowS :: ToVn a => Mode -> Depth -> Switch -> a -> STM ShowS
toShowS m d switch = printTree (switches conf) . toVn
  where
    switches = d                           -- finally depth
             . applySwitch (modeSwitch m)  -- next the mode
             . applySwitch switch          -- options come first

-- | Works like toShowS, but returns String instead of ShowS
toString :: ToVn a => Mode -> Depth -> Switch -> a -> STM String
toString m d switch = liftM evalShowS . toShowS m d switch
  where evalShowS s = s ""
