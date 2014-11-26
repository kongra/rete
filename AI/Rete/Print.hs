{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
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
module AI.Rete.Print where

import AI.Rete.Algo
import AI.Rete.Data
import Control.Concurrent.STM
import Control.Monad (liftM)
import Data.Foldable (Foldable)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Tree.Print
import Kask.Control.Monad (toListM, mapMM)
import Kask.Data.Function (compose, rcompose)

-- Vns (VISUALIZATION NODEs)

class ToVn a where
  toAdjsVn :: a -> AdjsVn
  toShowVn :: a -> ShowVn

toVn :: ToVn a => a -> Vn
toVn x = Vn { vnShowM = toShowVn x , vnAdjs  = toAdjsVn x }
{-# INLINE toVn #-}

type ShowVn = Opts -> STM ShowS
type AdjsVn = Opts -> STM [Vn]

data Vn =
  Vn { vnShowM :: !ShowVn
     , vnAdjs  :: !AdjsVn}

instance ShowM STM Opts Vn where showM o Vn { vnShowM = f } = f o

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
idS id' = compose [showString "-", showString $ show id']
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

netPropVn :: Opts -> OptPropVn
netPropVn Opts { oDataMode = dm } = if dm then optLeafPropVn else optPropVn
{-# INLINE netPropVn #-}

dataPropVn :: Opts -> OptPropVn
dataPropVn Opts { oDataMode = dm } = if dm then optPropVn else optLeafPropVn
{-# INLINE dataPropVn #-}

-- CONFIGURATION

type VConf = Conf STM ShowS Opts Vn

conf :: VConf
conf = Conf { impl     = stmImpl
            , adjs     = \o Vn { vnAdjs = f } -> f o
            , maxDepth = Nothing
            , opts     = defaultOpts }

-- | Sets the maxDepth of a configuration to the specified value.
depth :: Int -> VConf -> VConf
depth d c = c { maxDepth = Just d }
{-# INLINE depth #-}

-- | Unlimits the maxDepth of a configuration.
boundless :: VConf -> VConf
boundless c = c { maxDepth = Nothing }
{-# INLINE boundless #-}

-- STM IMPL

stmImpl :: Impl STM ShowS
stmImpl = str

-- OPTIONS

data Opts =
  Opts
  { oDataMode                 :: !Bool
  , oSymbolIds                :: !Bool

  , oWmeIds                   :: !Bool
  , oWmeSymbolic              :: !Bool
  , oWmeAmems                 :: !Bool
  , oWmeToks                  :: !Bool
  , oWmeNegativeJoinResults   :: !Bool

  , oTokIds                   :: !Bool
  , oTokWmes                  :: !Bool
  , oTokWmesSymbolic          :: !Bool
  , oTokParent                :: !Bool
  , oTokNode                  :: !Bool
  , oTokChildren              :: !Bool
  , oTokJoinResults           :: !Bool
  , oTokNccResults            :: !Bool
  , oTokOwner                 :: !Bool

  , oAmemFields               :: !Bool
  , oAmemRefcount             :: !Bool
  , oAmemWmes                 :: !Bool
  , oAmemSuccessors           :: !Bool

  , oNodeIds                  :: !Bool
  , oNodeParent               :: !Bool
  , oNodeChildren             :: !Bool

  , oUl                       :: !Bool

  , oBmemToks                 :: !Bool
  , oBmemAllChildren          :: !Bool

  , oJoinTests                :: !Bool
  , oJoinAmems                :: !Bool
  , oJoinNearestAncestors     :: !Bool

  , oNegativeTests            :: !Bool
  , oNegativeAmems            :: !Bool
  , oNegativeNearestAncestors :: !Bool
  , oNegativeToks             :: !Bool

  , oNccConjucts              :: !Bool
  , oNccPartners              :: !Bool
  , oNccNodes                 :: !Bool
  , oNccToks                  :: !Bool
  , oNccNewResultBuffers      :: !Bool

  , oPToks                    :: !Bool
  , oPLocations               :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
  { oDataMode                 = False
  , oSymbolIds                = False

  , oWmeIds                   = False
  , oWmeSymbolic              = False
  , oWmeAmems                 = False
  , oWmeToks                  = False
  , oWmeNegativeJoinResults   = False

  , oTokIds                   = False
  , oTokWmes                  = False
  , oTokWmesSymbolic          = False
  , oTokParent                = False
  , oTokNode                  = False
  , oTokChildren              = False
  , oTokJoinResults           = False
  , oTokNccResults            = False
  , oTokOwner                 = False

  , oAmemFields               = False
  , oAmemRefcount             = False
  , oAmemWmes                 = False
  , oAmemSuccessors           = False

  , oNodeIds                  = False
  , oNodeParent               = False
  , oNodeChildren             = False
  , oUl                       = False

  , oBmemToks                 = False
  , oBmemAllChildren          = False

  , oJoinTests                = False
  , oJoinAmems                = False
  , oJoinNearestAncestors     = False

  , oNegativeTests            = False
  , oNegativeAmems            = False
  , oNegativeNearestAncestors = False
  , oNegativeToks             = False

  , oNccConjucts              = False
  , oNccPartners              = False
  , oNccNodes                 = False
  , oNccToks                  = False
  , oNccNewResultBuffers      = False

  , oPToks                    = False
  , oPLocations               = False }

type Oswitch = Opts -> Opts

-- META OPTS.

withIds, noIds :: Oswitch
withIds = withSymbolIds
        . withWmeIds
        . withTokIds
        . withNodeIds
noIds   = noSymbolIds
        . noWmeIds
        . noTokIds
        . noNodeIds

withToks, noToks :: Oswitch
withToks = withWmeToks . withNodeToks
noToks   = noWmeToks   . noNodeToks

withNodeToks, noNodeToks :: Oswitch
withNodeToks = withBmemToks
             . withNegativeToks
             . withNccToks
             . withPToks
noNodeToks   = noBmemToks
             . noNegativeToks
             . noNccToks
             . noPToks

withTests, noTests :: Oswitch
withTests = withJoinTests    . withNegativeTests
noTests   = noJoinTests . noNegativeTests

withNodeAmems, noNodeAmems :: Oswitch
withNodeAmems = withJoinAmems . withNegativeAmems
noNodeAmems   = noJoinAmems   . noNegativeAmems

withAmems, noAmems :: Oswitch
withAmems = withWmeAmems . withNodeAmems
noAmems   = noWmeAmems   . noNodeAmems

withAllWmesSymbolic, withAllWmesExplicit :: Oswitch
withAllWmesSymbolic = withWmesSymbolic
                    . withTokWmesSymbolic
withAllWmesExplicit = withWmesExplicit
                    . withTokWmesExplicit

-- SPECIFIC OPTS.

inDataMode, inNetMode :: Oswitch
inDataMode o = o { oDataMode = True  }
inNetMode  o = o { oDataMode = False }

withSymbolIds, noSymbolIds :: Oswitch
withSymbolIds o = o { oSymbolIds = True  }
noSymbolIds   o = o { oSymbolIds = False }

withWmeIds, noWmeIds :: Oswitch
withWmeIds o = o { oWmeIds = True  }
noWmeIds   o = o { oWmeIds = False }

withWmesSymbolic, withWmesExplicit :: Oswitch
withWmesSymbolic o = o { oWmeSymbolic = True }
withWmesExplicit o = o { oWmeSymbolic = False }

withWmeAmems, noWmeAmems :: Oswitch
withWmeAmems o = o { oWmeAmems = True  }
noWmeAmems   o = o { oWmeAmems = False }

withWmeToks, noWmeToks :: Oswitch
withWmeToks o = o { oWmeToks = True  }
noWmeToks   o = o { oWmeToks = False }

withWmeNegativeJoinResults, noWmeNegativeJoinResults :: Oswitch
withWmeNegativeJoinResults o = o { oWmeNegativeJoinResults = True  }
noWmeNegativeJoinResults   o = o { oWmeNegativeJoinResults = False }

withTokIds, noTokIds :: Oswitch
withTokIds o = o { oTokIds = True  }
noTokIds   o = o { oTokIds = False }

withTokWmes, noTokWmes :: Oswitch
withTokWmes o = o { oTokWmes = True  }
noTokWmes   o = o { oTokWmes = False }

withTokWmesSymbolic, withTokWmesExplicit :: Oswitch
withTokWmesSymbolic o = o { oTokWmesSymbolic = True }
withTokWmesExplicit o = o { oTokWmesSymbolic = False }

withTokParent, noTokParent :: Oswitch
withTokParent o = o { oTokParent = True  }
noTokParent   o = o { oTokParent = False }

withTokNode, noTokNode :: Oswitch
withTokNode o = o { oTokNode = True  }
noTokNode   o = o { oTokNode = False }

withTokChildren, noTokChildren :: Oswitch
withTokChildren o = o { oTokChildren = True  }
noTokChildren   o = o { oTokChildren = False }

withTokJoinResults, noTokJoinResults :: Oswitch
withTokJoinResults o = o { oTokJoinResults = True  }
noTokJoinResults   o = o { oTokJoinResults = False }

withTokNccResults, noTokNccResults :: Oswitch
withTokNccResults o = o { oTokNccResults = True  }
noTokNccResults   o = o { oTokNccResults = False }

withTokOwner, noTokOwner :: Oswitch
withTokOwner o = o { oTokOwner = True  }
noTokOwner   o = o { oTokOwner = False }

withAmemFields, noAmemFields :: Oswitch
withAmemFields o = o { oAmemFields = True  }
noAmemFields   o = o { oAmemFields = False }

withAmemRefcount, noAmemRefcount :: Oswitch
withAmemRefcount o = o { oAmemRefcount = True  }
noAmemRefcount   o = o { oAmemRefcount = False }

withAmemWmes, noAmemWmes :: Oswitch
withAmemWmes o = o { oAmemWmes = True  }
noAmemWmes   o = o { oAmemWmes = False }

withAmemSuccessors, noAmemSuccessors :: Oswitch
withAmemSuccessors o = o { oAmemSuccessors = True  }
noAmemSuccessors   o = o { oAmemSuccessors = False }

withNodeIds, noNodeIds :: Oswitch
withNodeIds o = o { oNodeIds = True  }
noNodeIds   o = o { oNodeIds = False }

withUl, noUl :: Oswitch
withUl o = o { oUl = True  }
noUl   o = o { oUl = False }

withBmemToks, noBmemToks :: Oswitch
withBmemToks o = o { oBmemToks = True  }
noBmemToks   o = o { oBmemToks = False }

withBmemAllChildren, noBmemAllChildren :: Oswitch
withBmemAllChildren o = o { oBmemAllChildren = True  }
noBmemAllChildren   o = o { oBmemAllChildren = False }

withJoinTests, noJoinTests :: Oswitch
withJoinTests o = o { oJoinTests = True  }
noJoinTests   o = o { oJoinTests = False }

withJoinAmems, noJoinAmems :: Oswitch
withJoinAmems o = o { oJoinAmems = True  }
noJoinAmems   o = o { oJoinAmems = False }

withJoinNearestAncestors, noJoinNearestAncestors :: Oswitch
withJoinNearestAncestors o = o { oJoinNearestAncestors = True  }
noJoinNearestAncestors   o = o { oJoinNearestAncestors = False }

withNegativeTests, noNegativeTests :: Oswitch
withNegativeTests o = o { oNegativeTests = True  }
noNegativeTests   o = o { oNegativeTests = False }

withNegativeAmems, noNegativeAmems :: Oswitch
withNegativeAmems o = o { oNegativeAmems = True  }
noNegativeAmems   o = o { oNegativeAmems = False }

withNegativeNearestAncestors, noNegativeNearestAncestors :: Oswitch
withNegativeNearestAncestors o = o { oNegativeNearestAncestors = True  }
noNegativeNearestAncestors   o = o { oNegativeNearestAncestors = False }

withNegativeToks, noNegativeToks :: Oswitch
withNegativeToks o = o { oNegativeToks = True  }
noNegativeToks   o = o { oNegativeToks = False }

withNccConjucts, noNccConjucts :: Oswitch
withNccConjucts o = o { oNccConjucts = True  }
noNccConjucts   o = o { oNccConjucts = False }

withNccPartners, noNccPartners :: Oswitch
withNccPartners o = o { oNccPartners = True  }
noNccPartners   o = o { oNccPartners = False }

withNccNodes, noNccNodes :: Oswitch
withNccNodes o = o { oNccNodes = True  }
noNccNodes   o = o { oNccNodes = False }

withNccToks, noNccToks :: Oswitch
withNccToks o = o { oNccToks = True  }
noNccToks   o = o { oNccToks = False }

withNccNewResultBuffers, noNccNewResultBuffers :: Oswitch
withNccNewResultBuffers o = o { oNccNewResultBuffers = True  }
noNccNewResultBuffers   o = o { oNccNewResultBuffers = False }

withPToks, noPToks :: Oswitch
withPToks o = o { oPToks = True  }
noPToks   o = o { oPToks = False }

withPLocations, noPLocations :: Oswitch
withPLocations o = o { oPLocations = True  }
noPLocations   o = o { oPLocations = False }

-- WMES VISUALIZATION

instance ToVn Wme where
  toAdjsVn = adjsWme
  toShowVn = showWme

showWme :: Wme -> Opts -> STM ShowS
showWme wme Opts { oWmeSymbolic = osymbolic, oWmeIds = oids } =
  if osymbolic
    then return (showWmeSymbolic wme)
    else return (showWmeExplicit oids wme)
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

adjsWme :: Wme -> Opts -> STM [Vn]
adjsWme
  Wme        { wmeAmems                = amems
             , wmeTokens               = toks
             , wmeNegJoinResults       = jresults}
  opts'@Opts { oWmeAmems               = oamems
             , oWmeToks                = otoks
             , oWmeNegativeJoinResults = ojresults} = do

    amemVn <- netPropVn  opts' oamems    "amems" (readTVar amems)
    toksVn <- dataPropVn opts' otoks     "toks"   (readTVar toks)
    njrsVn <- dataPropVn opts' ojresults "neg. ⊳⊲ results (owners)"
              -- When visualizing the negative join results we only
              -- show the owner tokens, cause wme in every negative join
              -- result is this wme.
              (mapMM (return . negativeJoinResultOwner) (toListT jresults))

    optVns [amemVn, toksVn, njrsVn]
{-# INLINE adjsWme #-}

-- TOKENS VISUALIZATION

instance ToVn Token where
  toAdjsVn = adjsTok
  toShowVn = showTok

showTok :: Token -> Opts -> STM ShowS
showTok
  DummyTopToken {}
  Opts { oTokIds = oids } = return (withOptIdS oids (showString "{}") (-1))

showTok
  tok
  Opts { oWmeIds          = owmeids
       , oTokIds          = oids
       , oTokWmes         = owmes
       , oTokWmesSymbolic = osymbolic } = do
    let s = if owmes
              then (if osymbolic
                    then showTokWmesSymbolic tok
                    else showTokWmesExplicit owmeids tok)
              else showString "{..}"
    return (withOptIdS oids s (tokId tok))
{-# INLINE showTok #-}

showTokWmesSymbolic :: Token -> ShowS
showTokWmesSymbolic = showTokWmes showWmeSymbolic
{-# INLINE showTokWmesSymbolic #-}

showTokWmesExplicit :: Bool -> Token -> ShowS
showTokWmesExplicit owmeids = showTokWmes (showWmeExplicit owmeids)
{-# INLINE showTokWmesExplicit #-}

showTokWmes :: (Wme -> ShowS) -> Token -> ShowS
showTokWmes f = rcompose
              . intersperse (showString ",")
              . map (showWmeMaybe f)
              . tokWmes
{-# INLINE showTokWmes #-}

adjsTok :: Token -> Opts -> STM [Vn]
adjsTok
  DummyTopToken { tokNode  = node,  tokChildren  = children  }
  opts'@Opts    { oTokNode = onode, oTokChildren = ochildren } = do
    nodeVn     <- netPropVn  opts' onode     "node"     (return [node])
    childrenVn <- dataPropVn opts' ochildren "children" (readTVar children)
    optVns [nodeVn, childrenVn]

adjsTok
  Token       { tokParent         = parent
              , tokOwner          = mowner
              , tokNode           = node
              , tokChildren       = children
              , tokNegJoinResults = jresults
              , tokNccResults     = nresults }
  opts'@Opts  { oTokParent        = oparent
              , oTokOwner         = oowner
              , oTokNode          = onode
              , oTokChildren      = ochildren
              , oTokJoinResults   = ojresults -- map to wme
              , oTokNccResults    = onresults } = do

    parentVn <- dataPropVn opts' oparent "parent" (return [parent])
    ownerVn  <- dataPropVn opts' oowner  "owner"  (liftM owner (readTVar mowner))
    nodeVn   <- netPropVn  opts' onode   "node"   (return [node])

    childrenVn <- dataPropVn opts' ochildren "children" (readTVar children)
    jresultsVn <- dataPropVn opts' ojresults "neg. ⊳⊲ results (wmes)"
                  -- When visualizing the negative join results we only
                  -- show the wmes, cause owner in every negative join
                  -- result is this tok(en).
                  (mapMM (return . negativeJoinResultWme) (toListT jresults))
    nresultsVn <- dataPropVn opts' onresults "ncc results" (readTVar nresults)
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

showAmem :: Amem -> Opts -> STM ShowS
showAmem
  Amem { amemObj            = obj
       , amemAttr           = attr
       , amemVal            = val
       , amemReferenceCount = rcount }
  Opts { oAmemFields        = ofields
       , oAmemRefcount      = orc } = do
    let alpha = showString "α"
    let repr  = if ofields
                  then compose [alpha, showString " ("
                                , sS obj,  showString ","
                                , sS attr, showString ","
                                , sS val
                                ,  showString ")"]
                  else alpha
    if orc
      then (do rc <- readTVar rcount
               return $ compose [repr, showString " refcount ", shows rc])
      else return repr
  where
    sS s | s == wildcardSymbol = showString "*"
         | otherwise           = shows s
{-# INLINE showAmem #-}

adjsAmem :: Amem -> Opts -> STM [Vn]
adjsAmem
  Amem       { amemSuccessors  = succ'
             , amemWmes        = wmes }
  opts'@Opts { oAmemSuccessors = osucc
             , oAmemWmes       = owmes } = do
    succVn <- netPropVn  opts' osucc "successors" (readTVar succ')
    wmesVn <- dataPropVn opts' owmes "wmes"       (readTVar wmes)
    optVns [succVn, wmesVn]
{-# INLINE adjsAmem #-}

-- NODE VISUALIZATION

instance ToVn Node where
  toAdjsVn = adjsNode
  toShowVn = showNode

showNode :: Node -> Opts -> STM ShowS
showNode DummyTopNode {} Opts { oNodeIds = oids } =
  return (withOptIdS oids (showString "DTN (β)") (-1))

showNode node opts' = do
  let variant = nodeVariant node
  s <- case variant of
    Bmem         {} -> showBmem         variant opts'
    JoinNode     {} -> showJoinNode     variant opts'
    NegativeNode {} -> showNegativeNode variant opts'
    NccNode      {} -> showNccNode      variant opts'
    NccPartner   {} -> showNccPartner   variant opts'
    PNode        {} -> showPNode        variant opts'
    DTN          {} -> unreachableCode

  return (withOptIdS (oNodeIds opts') s (nodeId node))
{-# INLINE showNode #-}

adjsNode :: Node -> Opts -> STM [Vn]
adjsNode
  DummyTopNode { nodeChildren  = children
               , nodeVariant   = variant }
  opts'@Opts   { oNodeChildren = ochildren } = do
    childrenVn <- netPropVn opts' ochildren "children" (readTVar children)
    variantVns <- adjsDTN variant opts'
    optVns (variantVns ++ [childrenVn])

adjsNode
  Node { nodeParent    = parent
       , nodeChildren  = children
       , nodeVariant   = variant }
  opts'@Opts   { oNodeChildren = ochildren } = do
    childrenVn <- netPropVn opts' ochildren "children" (readTVar children)
    parentVn   <- netPropVn opts' ochildren "parent"   (return [parent])
    variantVns <- case variant of
      Bmem         {} -> adjsBmem         variant opts'
      JoinNode     {} -> adjsJoinNode     variant opts'
      NegativeNode {} -> adjsNegativeNode variant opts'
      NccNode      {} -> adjsNccNode      variant opts'
      NccPartner   {} -> adjsNccPartner   variant opts'
      PNode        {} -> adjsPNode        variant opts'
      DTN          {} -> unreachableCode

    optVns (variantVns ++ [parentVn, childrenVn])
{-# INLINE adjsNode #-}

-- Bmem VISUALIZATION

showBmem :: NodeVariant -> Opts -> STM ShowS
showBmem _ _ = return (showString "β")
{-# INLINE showBmem #-}

adjsBmemLike :: TSet Token -> TSet Node -> Opts -> STM [Maybe Vn]
adjsBmemLike toks achildren
  opts'@Opts { oBmemToks  = otoks, oBmemAllChildren = oachildren } = do
    achildrenVn <- netPropVn  opts' oachildren "all children" (readTVar achildren)
    toksVn      <- dataPropVn opts' otoks "toks" (readTVar toks)
    return [achildrenVn, toksVn]
{-# INLINE adjsBmemLike #-}

adjsBmem :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsBmem Bmem { nodeTokens = toks,  bmemAllChildren  = achildren  } opts' =
  adjsBmemLike toks achildren opts'
adjsBmem _ _ = unreachableCode
{-# INLINE adjsBmem #-}

-- DTN VISUALIZATION

adjsDTN :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsDTN DTN { nodeTokens = toks,  bmemAllChildren  = achildren  } opts' =
  adjsBmemLike toks achildren opts'
adjsDTN _ _ = unreachableCode
{-# INLINE adjsDTN #-}

-- JoinNode VISUALIZATION

showJoinNode :: NodeVariant -> Opts -> STM ShowS
showJoinNode
  JoinNode { leftUnlinked = lu, rightUnlinked = ru }
  Opts     { oUl = oul } =
    if oul
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

adjsJoinNode :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsJoinNode
  JoinNode   { joinTests                   = tests
             , nodeAmem                    = amem
             , nearestAncestorWithSameAmem = ancestor }
  opts'@Opts { oJoinTests                  = otests
             , oJoinAmems                  = oamems
             , oJoinNearestAncestors       = oancestors } = do

    testsVn    <- netPropVn opts' otests     "tests" (return tests)
    amemVn     <- netPropVn opts' oamems     "amem"  (return [amem])
    ancestorVn <- netPropVn opts' oancestors "ancestor"
                  (joinAncestorM ancestor)

    return [amemVn, ancestorVn, testsVn]

adjsJoinNode _ _ = unreachableCode
{-# INLINABLE adjsJoinNode #-}

joinAncestorM :: Monad m => Maybe a -> m [a]
joinAncestorM ancestor = case ancestor of
  Nothing -> return []
  Just a  -> return [a]
{-# INLINE joinAncestorM #-}

-- NegativeNode VISUALIZATION

showNegativeNode :: NodeVariant -> Opts -> STM ShowS
showNegativeNode
  JoinNode { rightUnlinked = ru } Opts { oUl = oul } =
    if oul
      then (do mark <- ulSingleMark ru
               return (showString ('¬':' ':mark)))
      else return (showString "¬")

showNegativeNode _ _ = unreachableCode
{-# INLINE showNegativeNode #-}

adjsNegativeNode :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsNegativeNode
  NegativeNode { joinTests                   = tests
               , nodeAmem                    = amem
               , nearestAncestorWithSameAmem = ancestor
               , nodeTokens                  = toks}
  opts'@Opts   { oNegativeTests              = otests
               , oNegativeAmems              = oamems
               , oNegativeNearestAncestors   = oancestors
               , oNegativeToks               = otoks } = do

    amemVn     <- netPropVn opts' oamems     "amem"  (return [amem])
    testsVn    <- netPropVn opts' otests     "tests" (return tests)
    ancestorVn <- netPropVn opts' oancestors "ancestor"
                  (joinAncestorM ancestor)
    toksVn     <- dataPropVn opts' otoks "toks" (readTVar toks)

    return [amemVn, ancestorVn, testsVn, toksVn]

adjsNegativeNode _ _ = unreachableCode

-- NccNode VISUALIZATION

showNccNode :: NodeVariant -> Opts -> STM ShowS
showNccNode NccNode {} _ = return (showString "Ncc")
showNccNode _          _ = unreachableCode
{-# INLINE showNccNode #-}

adjsNccNode :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsNccNode
  NccNode    { nodeTokens = toks,  nccPartner   = partner }
  opts'@Opts { oNccToks   = otoks, oNccPartners = opartners } = do
    partnerVn <- netPropVn  opts' opartners "partner" (return [partner])
    toksVn    <- dataPropVn opts' otoks     "toks"    (readTVar toks)

    return [partnerVn, toksVn]

adjsNccNode _ _ = unreachableCode
{-# INLINE adjsNccNode #-}

-- NccPartner VISUALIZATION

showNccPartner :: NodeVariant -> Opts -> STM ShowS
showNccPartner
  NccPartner { nccPartnerNumberOfConjucts = conjs  }
  Opts       { oNccConjucts               = oconjs } =
    if oconjs
      then return (compose [showString "Ncc (P) conjucts ", shows conjs])
      else return (showString "Ncc (P)")

showNccPartner _ _ = unreachableCode

-- oNccNodes
-- oNccNewResultBuffers
adjsNccPartner :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsNccPartner = undefined

-- PNode VISUALIZATION
-- oPToks
-- oPLocations
showPNode :: NodeVariant -> Opts -> STM ShowS
showPNode = undefined

adjsPNode :: NodeVariant -> Opts -> STM [Maybe Vn]
adjsPNode = undefined

-- JoinTest VISUALIZATION

instance ToVn JoinTest where
  toAdjsVn = adjsJoinTest
  toShowVn = showJoinTest

showJoinTest :: JoinTest -> Opts -> STM ShowS
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

adjsJoinTest :: JoinTest -> Opts -> STM [Vn]
adjsJoinTest _ _ = return []
{-# INLINE adjsJoinTest #-}

-- UTILS

unreachableCode :: a
unreachableCode = error "Unreachable code. Impossible has happened!!!"
{-# INLINE unreachableCode #-}
