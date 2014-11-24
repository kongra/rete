{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- WHAT TO VISUALIZE (type s)
-- wmes
-- toks
-- amems
-- Nodes with NodeVariants
--   bmems
--   join nodes
--   negative nodes
--   ncc nodes
--   ncc partners
--   p-nodes
--   possibly other kinds of nodes ...

-- WHAT WE ACTUALLY NEED FOR s
-- tree+, tree- - THE API
-- tree+Impl, tree-Impl - IMPLEMENTATIONS

-- Vn CONSTRUCTION (ABSTRACTION)

class ToVn a where
  toVn     :: a -> Vn
  toShowVn :: a -> ShowVn

-- VIS.-TREE NODES

type ShowVn = Opts -> STM ShowS
type AdjsVn = Opts -> STM [Vn]

-- | A Visible Node (hence Vn), the basic data structure for the tree
-- visualization process, compatible with the treeprint API.
data Vn =
  Vn { vnShowM :: !ShowVn
     , vnAdjsU :: !AdjsVn
     , vnAdjsD :: !AdjsVn }

instance ShowM STM Opts Vn where showM o Vn { vnShowM = f } = f o

-- LEAF/PROPERTY Vns CREATION

emptyAdjs :: Monad m => a -> m [t]
emptyAdjs = return . const []
{-# INLINE emptyAdjs #-}

leafVn :: ShowVn -> Vn
leafVn svn = Vn { vnShowM = svn
                , vnAdjsU = emptyAdjs
                , vnAdjsD = emptyAdjs }
{-# INLINE leafVn #-}

propVn :: ShowS -> [Vn] -> Vn
propVn name vns = Vn { vnShowM = s
                     , vnAdjsU = a
                     , vnAdjsD = a }
  where
    s _ = return name
    a = return . const vns
{-# INLINE propVn #-}

leafPropVn :: ShowS -> [ShowVn] -> Vn
leafPropVn name svns = propVn name (map leafVn svns)
{- INLINE leafPropVn -}

showS :: Show a => a -> ShowS
showS = showString . show
{-# INLINE showS #-}

idS :: ID -> ShowS
idS id' = compose [showS "-", showS id']
{-# INLINE idS #-}

withIdS :: ShowS -> ID -> ShowS
withIdS s id' = compose [s, idS id']
{-# INLINE withIdS #-}

withOptIdS :: Bool -> ShowS -> ID -> ShowS
withOptIdS False s _   = s
withOptIdS True  s id' = s `withIdS` id'
{-# INLINE withOptIdS #-}

toShowVnsM :: (Monad m, Foldable f, ToVn a) => m (f a) -> m [ShowVn]
toShowVnsM = liftM (map toShowVn) . toListM
{-# INLINE toShowVnsM #-}

optLeafPropVn :: (Monad m, Foldable f, ToVn a) =>
                 Bool
              -> String
              -> m (f a)
              -> m (Maybe Vn)
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

-- CONFIGURATIONS FOR TRAVERSING UP/DOWN

adjsU, adjsD :: Adjs STM Opts Vn
adjsU o Vn { vnAdjsU = f } = f o
{-# INLINE adjsU #-}
adjsD o Vn { vnAdjsD = f } = f o
{-# INLINE adjsD #-}

type VConf = Conf STM ShowS Opts Vn

confU, confD :: VConf
confU = Conf { impl     = stmImpl
             , adjs     = adjsU
             , maxDepth = Nothing
             , opts     = defaultOpts }

confD = confU { adjs = adjsD }

-- | Sets the maxDepth of a configuration to the specified value.
depth :: Int -> VConf -> VConf
depth d conf = conf { maxDepth = Just d }
{-# INLINE depth #-}

-- | Unlimits the maxDepth of a configuration.
boundless :: VConf -> VConf
boundless conf = conf { maxDepth = Nothing }
{-# INLINE boundless #-}

-- PRINTING STRINGS

instance ShowM STM Opts Symbol where
  showM Opts {oSymbolIds = ids} s =
    if ids
      then return (compose [ showString (show (getId s))
                           , showString ('-':show s)])
      else return (showString (show s))

-- STM IMPL

stmImpl :: Impl STM ShowS
stmImpl = str

-- OPTIONS

data Opts =
  Opts
  {
    oSymbolIds                :: !Bool

  , oWmeIds                   :: !Bool
  , oWmeSymbolic              :: !Bool
  , oWmeAmems                 :: !Bool
  , oWmeToks                  :: !Bool
  , oWmeNegativeJoinResults   :: !Bool

  , oTokIds                   :: !Bool
  , oTokWmes                  :: !Bool
  , oTokWmesSymbolic          :: !Bool
  , oTokParents               :: !Bool
  , oTokNodes                 :: !Bool
  , oTokChildren              :: !Bool
  , oTokJoinResults           :: !Bool
  , oTokNccResults            :: !Bool
  , oTokOwners                :: !Bool

  , oAmemFields               :: !Bool
  , oAmemRefcount             :: !Bool
  , oAmemWmes                 :: !Bool
  , oAmemWmesSymbolic         :: !Bool

  , oNodeIds                  :: !Bool
  , oUl                       :: !Bool

  , oBmemToks                 :: !Bool

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
  { oSymbolIds                = False

  , oWmeIds                   = False
  , oWmeSymbolic              = True
  , oWmeAmems                 = False
  , oWmeToks                  = False
  , oWmeNegativeJoinResults   = False

  , oTokIds                   = False
  , oTokWmes                  = False
  , oTokWmesSymbolic          = True
  , oTokParents               = False
  , oTokNodes                 = False
  , oTokChildren              = False
  , oTokJoinResults           = False
  , oTokNccResults            = False
  , oTokOwners                = False

  , oAmemFields               = True
  , oAmemRefcount             = False
  , oAmemWmes                 = False
  , oAmemWmesSymbolic         = True

  , oNodeIds                  = False
  , oUl                       = True

  , oBmemToks                 = False

  , oJoinTests                = True
  , oJoinAmems                = True
  , oJoinNearestAncestors     = False

  , oNegativeTests            = True
  , oNegativeAmems            = True
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
                    . withAmemWmesSymbolic
withAllWmesExplicit = withWmesExplicit
                    . withTokWmesExplicit
                    . withAmemWmesExplicit

-- SPECIFIC OPTS.

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

withTokParents, noTokParents :: Oswitch
withTokParents o = o { oTokParents = True  }
noTokParents   o = o { oTokParents = False }

withTokNodes, noTokNodes :: Oswitch
withTokNodes o = o { oTokNodes = True  }
noTokNodes   o = o { oTokNodes = False }

withTokChildren, noTokChildren :: Oswitch
withTokChildren o = o { oTokChildren = True  }
noTokChildren   o = o { oTokChildren = False }

withTokJoinResults, noTokJoinResults :: Oswitch
withTokJoinResults o = o { oTokJoinResults = True  }
noTokJoinResults   o = o { oTokJoinResults = False }

withTokNccResults, noTokNccResults :: Oswitch
withTokNccResults o = o { oTokNccResults = True  }
noTokNccResults   o = o { oTokNccResults = False }

withTokOwners, noTokOwners :: Oswitch
withTokOwners o = o { oTokOwners = True  }
noTokOwners   o = o { oTokOwners = False }

withAmemFields, noAmemFields :: Oswitch
withAmemFields o = o { oAmemFields = True  }
noAmemFields   o = o { oAmemFields = False }

withAmemRefcount, noAmemRefcount :: Oswitch
withAmemRefcount o = o { oAmemRefcount = True  }
noAmemRefcount   o = o { oAmemRefcount = False }

withAmemWmes, noAmemWmes :: Oswitch
withAmemWmes o = o { oAmemWmes = True  }
noAmemWmes   o = o { oAmemWmes = False }

withAmemWmesSymbolic, withAmemWmesExplicit :: Oswitch
withAmemWmesSymbolic o = o { oAmemWmesSymbolic = True }
withAmemWmesExplicit o = o { oAmemWmesSymbolic = False }

withNodeIds, noNodeIds :: Oswitch
withNodeIds o = o { oNodeIds = True  }
noNodeIds   o = o { oNodeIds = False }

withUl, noUl :: Oswitch
withUl o = o { oUl = True  }
noUl   o = o { oUl = False }

withBmemToks, noBmemToks :: Oswitch
withBmemToks o = o { oBmemToks = True  }
noBmemToks   o = o { oBmemToks = False }

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
  toVn wme = Vn { vnShowM = showWme  wme
                , vnAdjsU = adjsWmeU wme
                , vnAdjsD = adjsWmeD wme }

  toShowVn = showWme

showWme :: Wme -> Opts -> STM ShowS
showWme wme Opts { oWmeSymbolic = osymbolic, oWmeIds = oids } =
  if osymbolic
    then return (showWmeSymbolic wme)
    else return (showWmeExplicit oids wme)
{-# INLINE showWme #-}

showWmeSymbolic :: Wme -> ShowS
showWmeSymbolic wme = compose [showS "w", showS $ wmeId wme]
{-# INLINE showWmeSymbolic #-}

showWmeExplicit :: Bool -> Wme -> ShowS
showWmeExplicit withId
  Wme { wmeId = id', wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    withOptIdS withId
      (compose [ showS "(", showS obj,  showS ","
               , showS attr, showS ",", showS val, showS ")"])
      id'
{-# INLINE showWmeExplicit #-}

showWmeMaybe :: (Wme -> ShowS) -> Maybe Wme -> ShowS
showWmeMaybe _ Nothing    = showS "_"
showWmeMaybe f (Just wme) = f wme
{-# INLINE showWmeMaybe #-}

adjsWmeU :: Wme -> Opts -> STM [Vn]
adjsWmeU = adjsWmeD

adjsWmeD :: Wme -> Opts -> STM [Vn]
adjsWmeD
  Wme  { wmeAmems                = amems
       , wmeTokens               = toks
       , wmeNegJoinResults       = njrs}
  Opts { oWmeAmems               = oamems
       , oWmeToks                = otoks
       , oWmeNegativeJoinResults = onjrs} = do

    amemVn <- optLeafPropVn oamems "α mems" (readTVar amems)
    toksVn <- optLeafPropVn otoks  "toks"   (readTVar toks)
    njrsVn <- optLeafPropVn onjrs  "neg. ⊳⊲ results (owners)"
              -- When visualizing the negative join results we only
              -- show the owner tokens, cause wme in every negative join
              -- result is this wme.
              (mapMM (return . negativeJoinResultOwner) (toListT njrs))

    optVns [amemVn, toksVn, njrsVn]

{-# INLINABLE adjsWmeD #-}

-- TOKENS VISUALIZATION

instance ToVn Token where
  toVn wme = Vn { vnShowM = showTok  wme
                , vnAdjsU = adjsTokU wme
                , vnAdjsD = adjsTokD wme }

  toShowVn = showTok

showTok :: Token -> Opts -> STM ShowS
showTok
  DummyTopToken {}
  Opts { oTokIds = oids } = return (withOptIdS oids (showS "⟨⟩") (-1))

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
              else showS "⟨..⟩"
    return (withOptIdS oids s (tokId tok))
{-# INLINABLE showTok #-}

showTokWmesSymbolic :: Token -> ShowS
showTokWmesSymbolic = showTokWmes showWmeSymbolic
{-# INLINE showTokWmesSymbolic #-}

showTokWmesExplicit :: Bool -> Token -> ShowS
showTokWmesExplicit owmeids = showTokWmes (showWmeExplicit owmeids)
{-# INLINE showTokWmesExplicit #-}

showTokWmes :: (Wme -> ShowS) -> Token -> ShowS
showTokWmes f = rcompose
              . intersperse (showS ",")
              . map (showWmeMaybe f)
              . tokWmes
{-# INLINE showTokWmes #-}

-- oTokParents
-- oTokNodes
-- oTokChildren
-- oTokJoinResults
-- oTokNccResults
-- oTokOwners

-- showWme
--   Wme  { wmeId        = id'
--        , wmeObj       = obj
--        , wmeAttr      = attr
--        , wmeVal       = val }
--   Opts { oWmeSymbolic = osymbolic
--        , oWmeIds      = owmeIds } =
--     if osymbolic
--       then return $ compose [wS, showString (show id')]
--       else do
--         let s = compose [lparenS
--                         , showS obj, commaS
--                         , showS attr, commaS
--                         , showS val, rparenS]
--         return $ if owmeIds then s `withIdS` id' else s

adjsTokU :: Token -> Opts -> STM [Vn]
adjsTokU = adjsTokD

adjsTokD :: Token -> Opts -> STM [Vn]
adjsTokD tok opts' = undefined


-- AMEMS VISUALIZATION

instance ToVn Amem where
  toVn     _ = undefined
  toShowVn _ = undefined
