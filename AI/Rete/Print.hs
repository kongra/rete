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

import AI.Rete.Algo (getId)
import AI.Rete.Data
import Control.Concurrent.STM
import Control.Monad (liftM)
import Data.Foldable (Foldable)
import Data.Tree.Print
import Kask.Control.Monad (toListM)
import Kask.Data.Function (compose)

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

-- LEAF/PROPERTY Vns

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
  showM Opts {optsSymbolIds = ids} s =
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
    optsSymbolIds                :: !Bool

  , optsWmeIds                   :: !Bool
  , optsWmeSymbolic              :: !Bool
  , optsWmeAmems                 :: !Bool
  , optsWmeToks                  :: !Bool
  , optsWmeNegativeJoinResults   :: !Bool

  , optsTokIds                   :: !Bool
  , optsTokWmes                  :: !Bool
  , optsTokWmesSymbolic          :: !Bool
  , optsTokParents               :: !Bool
  , optsTokNodes                 :: !Bool
  , optsTokChildren              :: !Bool
  , optsTokJoinResults           :: !Bool
  , optsTokNccResults            :: !Bool
  , optsTokOwners                :: !Bool

  , optsAmemFields               :: !Bool
  , optsAmemRefcount             :: !Bool
  , optsAmemWmes                 :: !Bool
  , optsAmemWmesSymbolic         :: !Bool

  , optsNodeIds                  :: !Bool
  , optsUl                       :: !Bool

  , optsBmemToks                 :: !Bool

  , optsJoinTests                :: !Bool
  , optsJoinAmems                :: !Bool
  , optsJoinNearestAncestors     :: !Bool

  , optsNegativeTests            :: !Bool
  , optsNegativeAmems            :: !Bool
  , optsNegativeNearestAncestors :: !Bool
  , optsNegativeToks             :: !Bool

  , optsNccConjucts              :: !Bool
  , optsNccPartners              :: !Bool
  , optsNccNodes                 :: !Bool
  , optsNccToks                  :: !Bool
  , optsNccNewResultBuffers      :: !Bool

  , optsPToks                    :: !Bool
  , optsPLocations               :: !Bool
  }

defaultOpts :: Opts
defaultOpts =
  Opts
  { optsSymbolIds                = False

  , optsWmeIds                   = False
  , optsWmeSymbolic              = True
  , optsWmeAmems                 = False
  , optsWmeToks                  = False
  , optsWmeNegativeJoinResults   = False

  , optsTokIds                   = False
  , optsTokWmes                  = False
  , optsTokWmesSymbolic          = True
  , optsTokParents               = False
  , optsTokNodes                 = False
  , optsTokChildren              = False
  , optsTokJoinResults           = False
  , optsTokNccResults            = False
  , optsTokOwners                = False

  , optsAmemFields               = True
  , optsAmemRefcount             = False
  , optsAmemWmes                 = False
  , optsAmemWmesSymbolic         = True

  , optsNodeIds                  = False
  , optsUl                       = True

  , optsBmemToks                 = False

  , optsJoinTests                = True
  , optsJoinAmems                = True
  , optsJoinNearestAncestors     = False

  , optsNegativeTests            = True
  , optsNegativeAmems            = True
  , optsNegativeNearestAncestors = False
  , optsNegativeToks             = False

  , optsNccConjucts              = False
  , optsNccPartners              = False
  , optsNccNodes                 = False
  , optsNccToks                  = False
  , optsNccNewResultBuffers      = False

  , optsPToks                    = False
  , optsPLocations               = False }

-- META OPTS.

type Oswitch = Opts -> Opts

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

-- SPECIFIC OPTS.

withSymbolIds, noSymbolIds :: Oswitch
withSymbolIds o = o { optsSymbolIds = True  }
noSymbolIds   o = o { optsSymbolIds = False }

withWmeIds, noWmeIds :: Oswitch
withWmeIds o = o { optsWmeIds = True  }
noWmeIds   o = o { optsWmeIds = False }

withWmesSymbolic, withWmesExplicit :: Oswitch
withWmesSymbolic o = o { optsWmeSymbolic = True }
withWmesExplicit o = o { optsWmeSymbolic = False }

withWmeAmems, noWmeAmems :: Oswitch
withWmeAmems o = o { optsWmeAmems = True  }
noWmeAmems   o = o { optsWmeAmems = False }

withWmeToks, noWmeToks :: Oswitch
withWmeToks o = o { optsWmeToks = True  }
noWmeToks   o = o { optsWmeToks = False }

withWmeNegativeJoinResults, noWmeNegativeJoinResults :: Oswitch
withWmeNegativeJoinResults o = o { optsWmeNegativeJoinResults = True  }
noWmeNegativeJoinResults   o = o { optsWmeNegativeJoinResults = False }

withTokIds, noTokIds :: Oswitch
withTokIds o = o { optsTokIds = True  }
noTokIds   o = o { optsTokIds = False }

withTokWmes, noTokWmes :: Oswitch
withTokWmes o = o { optsTokWmes = True  }
noTokWmes   o = o { optsTokWmes = False }

withTokWmesSymbolic, withTokWmesExplicit :: Oswitch
withTokWmesSymbolic o = o { optsTokWmesSymbolic = True }
withTokWmesExplicit o = o { optsTokWmesSymbolic = False }

withTokParents, noTokParents :: Oswitch
withTokParents o = o { optsTokParents = True  }
noTokParents   o = o { optsTokParents = False }

withTokNodes, noTokNodes :: Oswitch
withTokNodes o = o { optsTokNodes = True  }
noTokNodes   o = o { optsTokNodes = False }

withTokChildren, noTokChildren :: Oswitch
withTokChildren o = o { optsTokChildren = True  }
noTokChildren   o = o { optsTokChildren = False }

withTokJoinResults, noTokJoinResults :: Oswitch
withTokJoinResults o = o { optsTokJoinResults = True  }
noTokJoinResults   o = o { optsTokJoinResults = False }

withTokNccResults, noTokNccResults :: Oswitch
withTokNccResults o = o { optsTokNccResults = True  }
noTokNccResults   o = o { optsTokNccResults = False }

withTokOwners, noTokOwners :: Oswitch
withTokOwners o = o { optsTokOwners = True  }
noTokOwners   o = o { optsTokOwners = False }

withAmemFields, noAmemFields :: Oswitch
withAmemFields o = o { optsAmemFields = True  }
noAmemFields   o = o { optsAmemFields = False }

withAmemRefcount, noAmemRefcount :: Oswitch
withAmemRefcount o = o { optsAmemRefcount = True  }
noAmemRefcount   o = o { optsAmemRefcount = False }

withAmemWmes, noAmemWmes :: Oswitch
withAmemWmes o = o { optsAmemWmes = True  }
noAmemWmes   o = o { optsAmemWmes = False }

withAmemWmesSymbolic, withAmemWmesExplicit :: Oswitch
withAmemWmesSymbolic o = o { optsAmemWmesSymbolic = True }
withAmemWmesExplicit o = o { optsAmemWmesSymbolic = False }

withNodeIds, noNodeIds :: Oswitch
withNodeIds o = o { optsNodeIds = True  }
noNodeIds   o = o { optsNodeIds = False }

withUl, noUl :: Oswitch
withUl o = o { optsUl = True  }
noUl   o = o { optsUl = False }

withBmemToks, noBmemToks :: Oswitch
withBmemToks o = o { optsBmemToks = True  }
noBmemToks   o = o { optsBmemToks = False }

withJoinTests, noJoinTests :: Oswitch
withJoinTests o = o { optsJoinTests = True  }
noJoinTests   o = o { optsJoinTests = False }

withJoinAmems, noJoinAmems :: Oswitch
withJoinAmems o = o { optsJoinAmems = True  }
noJoinAmems   o = o { optsJoinAmems = False }

withJoinNearestAncestors, noJoinNearestAncestors :: Oswitch
withJoinNearestAncestors o = o { optsJoinNearestAncestors = True  }
noJoinNearestAncestors   o = o { optsJoinNearestAncestors = False }

withNegativeTests, noNegativeTests :: Oswitch
withNegativeTests o = o { optsNegativeTests = True  }
noNegativeTests   o = o { optsNegativeTests = False }

withNegativeAmems, noNegativeAmems :: Oswitch
withNegativeAmems o = o { optsNegativeAmems = True  }
noNegativeAmems   o = o { optsNegativeAmems = False }

withNegativeNearestAncestors, noNegativeNearestAncestors :: Oswitch
withNegativeNearestAncestors o = o { optsNegativeNearestAncestors = True  }
noNegativeNearestAncestors   o = o { optsNegativeNearestAncestors = False }

withNegativeToks, noNegativeToks :: Oswitch
withNegativeToks o = o { optsNegativeToks = True  }
noNegativeToks   o = o { optsNegativeToks = False }

withNccConjucts, noNccConjucts :: Oswitch
withNccConjucts o = o { optsNccConjucts = True  }
noNccConjucts   o = o { optsNccConjucts = False }

withNccPartners, noNccPartners :: Oswitch
withNccPartners o = o { optsNccPartners = True  }
noNccPartners   o = o { optsNccPartners = False }

withNccNodes, noNccNodes :: Oswitch
withNccNodes o = o { optsNccNodes = True  }
noNccNodes   o = o { optsNccNodes = False }

withNccToks, noNccToks :: Oswitch
withNccToks o = o { optsNccToks = True  }
noNccToks   o = o { optsNccToks = False }

withNccNewResultBuffers, noNccNewResultBuffers :: Oswitch
withNccNewResultBuffers o = o { optsNccNewResultBuffers = True  }
noNccNewResultBuffers   o = o { optsNccNewResultBuffers = False }

withPToks, noPToks :: Oswitch
withPToks o = o { optsPToks = True  }
noPToks   o = o { optsPToks = False }

withPLocations, noPLocations :: Oswitch
withPLocations o = o { optsPLocations = True  }
noPLocations   o = o { optsPLocations = False }

-- WMES VISUALIZATION

instance ToVn Wme where
  toVn wme = Vn { vnShowM = showWme  wme
                , vnAdjsU = adjsWmeU wme
                , vnAdjsD = adjsWmeD wme }

  toShowVn = showWme

showWme :: Wme -> Opts -> STM ShowS
showWme
  Wme  { wmeId           = id'
       , wmeObj          = obj
       , wmeAttr         = attr
       , wmeVal          = val }
  Opts { optsWmeSymbolic = osymbolic
       , optsWmeIds      = owmeIds } =
    if osymbolic
      then return $ compose [wS, showString (show id')]
      else do
        let s = compose [lparenS
                        , showS obj, commaS
                        , showS attr, commaS
                        , showS val, rparenS]
        return $ if owmeIds then s `withIdS` id' else s

adjsWmeU :: Wme -> Opts -> STM [Vn]
adjsWmeU = adjsWmeD

adjsWmeD :: Wme -> Opts -> STM [Vn]
adjsWmeD _ _ = undefined
-- adjsWmeD
--   Wme  { wmeAmems                   = amems
--        , wmeTokens                  = toks
--        , wmeNegJoinResults          = nresults}
--   Opts { optsWmeAmems               = oamems
--        , optsWmeToks                = otoks
--        , optsWmeNegativeJoinResults = onresults} = do

--     amems' <- if oamems then (return []) else (return [])
--     -- toks'     <- if otoks     then tokNVs     else (return [])
--     -- nresults' <- if onresults then nresultNVs else (return [])
--     return amems'
--     where
--       label   = showString ":amems"
--       amemNVs = return (leafPropVn label (mapMM toShowVn (readTVar amems)))

--       -- toksS     = showString ":toks"
--       -- nresultsS = showString "neg. join results (owner)"

--       --
--       -- tokNVs     = leafPropVn toksS     (map toShowVn (readTVar toks))
--       -- nresultNVs = leafPropVn nresultsS (map toShowVn (readTVar nresults))

-- AMEMS VISUALIZATION

instance ToVn Amem where
  toVn     _ = undefined
  toShowVn _ = undefined

-- STRING CONSTANTS

commaS, commspcS :: ShowS
commaS   = showString ","
commspcS = showString ", "

wS :: ShowS
wS = showString "w"

lparenS, rparenS :: ShowS
lparenS = showString "("
rparenS = showString ")"

dashS :: ShowS
dashS = showString "-"

-- MISC. UTILITIES

showS :: Show a => a -> ShowS
showS = showString . show
{-# INLINE showS #-}

idS :: ID -> ShowS
idS id' = compose [dashS, showS id']
{-# INLINE idS #-}

withIdS :: ShowS -> ID -> ShowS
withIdS s id' = compose [s, idS id']
{-# INLINE withIdS #-}

toShowVnsM :: (Monad m, Foldable f, ToVn a) => m (f a) -> m [ShowVn]
toShowVnsM = liftM (map toShowVn) . toListM
{-# INLINE toShowVnsM #-}
