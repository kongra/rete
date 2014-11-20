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
import Data.Tree.Print
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
-- property nodes, flat nodes, flat property nodes
-- tree+, tree- - THE API
-- tree+Impl, tree-Impl - IMPLEMENTATIONS

-- VIS.-TREE NODES

data VN =
  VN { vnShowM :: !(Opts -> STM ShowS)
     , vnAdjsU :: !(Opts -> STM [VN])
     , vnAdjsD :: !(Opts -> STM [VN]) }

instance ShowM STM Opts VN where showM o VN { vnShowM = f } = f o

-- CONFIGURATIONS FOR TRAVERSING UP/DOWN

adjsU, adjsD :: Adjs STM Opts VN
adjsU o VN { vnAdjsU = f } = f o
{-# INLINE adjsU #-}
adjsD o VN { vnAdjsD = f } = f o
{-# INLINE adjsD #-}

type VConf = Conf STM ShowS Opts VN

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

-- STRING CONSTANTS
comma, commspc :: ShowS
comma   = showString ","
commspc = showString ", "

-- OPTIONS

data Opts =
  Opts
  {
    optsSymbolIds                :: !Bool

  , optsWmeIds                   :: !Bool
  , optsWmeAmems                 :: !Bool
  , optsWmeToks                  :: !Bool
  , optsWmeNegativeJoinResults   :: !Bool

  , optsTokIds                   :: !Bool
  , optsTokWmes                  :: !Bool
  , optsTokParents               :: !Bool
  , optsTokNodes                 :: !Bool
  , optsTokChildren              :: !Bool
  , optsTokJoinResults           :: !Bool
  , optsTokNccResults            :: !Bool
  , optsTokOwners                :: !Bool

  , optsAmemFields               :: !Bool
  , optsAmemRefcount             :: !Bool
  , optsAmemWmes                 :: !Bool

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
  , optsWmeAmems                 = False
  , optsWmeToks                  = False
  , optsWmeNegativeJoinResults   = False

  , optsTokIds                   = False
  , optsTokWmes                  = False
  , optsTokParents               = False
  , optsTokNodes                 = False
  , optsTokChildren              = False
  , optsTokJoinResults           = False
  , optsTokNccResults            = False
  , optsTokOwners                = False

  , optsAmemFields               = True
  , optsAmemRefcount             = True
  , optsAmemWmes                 = False

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
withIds, withoutIds :: Opts -> Opts
withIds    = withSymbolIds
           . withWmeIds
           . withTokIds
           . withNodeIds
withoutIds = withoutSymbolIds
           . withoutWmeIds
           . withoutTokIds
           . withoutNodeIds

withToks, withoutToks :: Opts -> Opts
withToks    = withWmeToks    . withNodeToks
withoutToks = withoutWmeToks . withoutNodeToks

withNodeToks, withoutNodeToks :: Opts -> Opts
withNodeToks    = withBmemToks
                . withNegativeToks
                . withNccToks
                . withPToks
withoutNodeToks = withoutBmemToks
                . withoutNegativeToks
                . withoutNccToks
                . withoutPToks

withTests, withoutTests :: Opts -> Opts
withTests    = withJoinTests    . withNegativeTests
withoutTests = withoutJoinTests . withoutNegativeTests

-- SPECIFIC OPTS.
withAmems, withoutAmems :: Opts -> Opts
withAmems    = withWmeAmems    . withJoinAmems    . withNegativeAmems
withoutAmems = withoutWmeAmems . withoutJoinAmems . withoutNegativeAmems

withSymbolIds, withoutSymbolIds :: Opts -> Opts
withSymbolIds    o = o { optsSymbolIds = True  }
withoutSymbolIds o = o { optsSymbolIds = False }

withWmeIds, withoutWmeIds :: Opts -> Opts
withWmeIds    o = o { optsWmeIds = True  }
withoutWmeIds o = o { optsWmeIds = False }

withWmeAmems, withoutWmeAmems :: Opts -> Opts
withWmeAmems    o = o { optsWmeAmems = True  }
withoutWmeAmems o = o { optsWmeAmems = False }

withWmeToks, withoutWmeToks :: Opts -> Opts
withWmeToks    o = o { optsWmeToks = True  }
withoutWmeToks o = o { optsWmeToks = False }

withWmeNegativeJoinResults, withoutWmeNegativeJoinResults :: Opts -> Opts
withWmeNegativeJoinResults    o = o { optsWmeNegativeJoinResults = True  }
withoutWmeNegativeJoinResults o = o { optsWmeNegativeJoinResults = False }

withTokIds, withoutTokIds :: Opts -> Opts
withTokIds    o = o { optsTokIds = True  }
withoutTokIds o = o { optsTokIds = False }

withTokWmes, withoutTokWmes :: Opts -> Opts
withTokWmes    o = o { optsTokWmes = True  }
withoutTokWmes o = o { optsTokWmes = False }

withTokParents, withoutTokParents :: Opts -> Opts
withTokParents    o = o { optsTokParents = True  }
withoutTokParents o = o { optsTokParents = False }

withTokNodes, withoutTokNodes :: Opts -> Opts
withTokNodes    o = o { optsTokNodes = True  }
withoutTokNodes o = o { optsTokNodes = False }

withTokChildren, withoutTokChildren :: Opts -> Opts
withTokChildren    o = o { optsTokChildren = True  }
withoutTokChildren o = o { optsTokChildren = False }

withTokJoinResults, withoutTokJoinResults :: Opts -> Opts
withTokJoinResults    o = o { optsTokJoinResults = True  }
withoutTokJoinResults o = o { optsTokJoinResults = False }

withTokNccResults, withoutTokNccResults :: Opts -> Opts
withTokNccResults    o = o { optsTokNccResults = True  }
withoutTokNccResults o = o { optsTokNccResults = False }

withTokOwners, withoutTokOwners :: Opts -> Opts
withTokOwners    o = o { optsTokOwners = True  }
withoutTokOwners o = o { optsTokOwners = False }

withAmemFields, withoutAmemFields :: Opts -> Opts
withAmemFields    o = o { optsAmemFields = True  }
withoutAmemFields o = o { optsAmemFields = False }

withAmemRefcount, withoutAmemRefcount :: Opts -> Opts
withAmemRefcount    o = o { optsAmemRefcount = True  }
withoutAmemRefcount o = o { optsAmemRefcount = False }

withAmemWmes, withoutAmemWmes :: Opts -> Opts
withAmemWmes    o = o { optsAmemWmes = True  }
withoutAmemWmes o = o { optsAmemWmes = False }

withNodeIds, withoutNodeIds :: Opts -> Opts
withNodeIds    o = o { optsNodeIds = True  }
withoutNodeIds o = o { optsNodeIds = False }

withUl, withoutUl :: Opts -> Opts
withUl    o = o { optsUl = True  }
withoutUl o = o { optsUl = False }

withBmemToks, withoutBmemToks :: Opts -> Opts
withBmemToks    o = o { optsBmemToks = True  }
withoutBmemToks o = o { optsBmemToks = False }

withJoinTests, withoutJoinTests :: Opts -> Opts
withJoinTests    o = o { optsJoinTests = True  }
withoutJoinTests o = o { optsJoinTests = False }

withJoinAmems, withoutJoinAmems :: Opts -> Opts
withJoinAmems    o = o { optsJoinAmems = True  }
withoutJoinAmems o = o { optsJoinAmems = False }

withJoinNearestAncestors, withoutJoinNearestAncestors :: Opts -> Opts
withJoinNearestAncestors    o = o { optsJoinNearestAncestors = True  }
withoutJoinNearestAncestors o = o { optsJoinNearestAncestors = False }

withNegativeTests, withoutNegativeTests :: Opts -> Opts
withNegativeTests    o = o { optsNegativeTests = True  }
withoutNegativeTests o = o { optsNegativeTests = False }

withNegativeAmems, withoutNegativeAmems :: Opts -> Opts
withNegativeAmems    o = o { optsNegativeAmems = True  }
withoutNegativeAmems o = o { optsNegativeAmems = False }

withNegativeNearestAncestors, withoutNegativeNearestAncestors :: Opts -> Opts
withNegativeNearestAncestors    o = o { optsNegativeNearestAncestors = True  }
withoutNegativeNearestAncestors o = o { optsNegativeNearestAncestors = False }

withNegativeToks, withoutNegativeToks :: Opts -> Opts
withNegativeToks    o = o { optsNegativeToks = True  }
withoutNegativeToks o = o { optsNegativeToks = False }

withNccConjucts, withoutNccConjucts :: Opts -> Opts
withNccConjucts    o = o { optsNccConjucts = True  }
withoutNccConjucts o = o { optsNccConjucts = False }

withNccPartners, withoutNccPartners :: Opts -> Opts
withNccPartners    o = o { optsNccPartners = True  }
withoutNccPartners o = o { optsNccPartners = False }

withNccNodes, withoutNccNodes :: Opts -> Opts
withNccNodes    o = o { optsNccNodes = True  }
withoutNccNodes o = o { optsNccNodes = False }

withNccToks, withoutNccToks :: Opts -> Opts
withNccToks    o = o { optsNccToks = True  }
withoutNccToks o = o { optsNccToks = False }

withNccNewResultBuffers, withoutNccNewResultBuffers :: Opts -> Opts
withNccNewResultBuffers    o = o { optsNccNewResultBuffers = True  }
withoutNccNewResultBuffers o = o { optsNccNewResultBuffers = False }

withPToks, withoutPToks :: Opts -> Opts
withPToks    o = o { optsPToks = True  }
withoutPToks o = o { optsPToks = False }

withPLocations, withoutPLocations :: Opts -> Opts
withPLocations    o = o { optsPLocations = True  }
withoutPLocations o = o { optsPLocations = False }
