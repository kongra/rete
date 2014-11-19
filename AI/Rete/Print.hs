{-# LANGUAGE CPP #-}
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

import Control.Concurrent.STM (STM)
import Data.Tree.Print (str, Impl)

-- STM IMPL

stms :: Impl STM ShowS
stms = str

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
  , optsAmemIds                  :: !Bool
  , optsAmemRefcount             :: !Bool
  , optsAmemWmes                 :: !Bool
  , optsBmemIds                  :: !Bool
  , optsBmemToks                 :: !Bool
  , optsUl                       :: !Bool
  , optsJoinIds                  :: !Bool
  , optsJoinTests                :: !Bool
  , optsJoinAmems                :: !Bool
  , optsJoinNearestAncestors     :: !Bool
  , optsNegativeIds              :: !Bool
  , optsNegativeTests            :: !Bool
  , optsNegativeAmems            :: !Bool
  , optsNegativeNearestAncestors :: !Bool
  , optsNegativeToks             :: !Bool
  , optsNccIds                   :: !Bool
  , optsNccConjucts              :: !Bool
  , optsNccPartners              :: !Bool
  , optsNccNodes                 :: !Bool
  , optsNccToks                  :: !Bool
  , optsNccNewResultBuffers      :: !Bool
  , optsPToks                    :: !Bool
  , optsPLocations               :: !Bool
  }

withSymbolIds, withoutSymbolIds :: Opts -> Opts
withSymbolIds    opts = opts { optsSymbolIds = True  }
withoutSymbolIds opts = opts { optsSymbolIds = False }

withWmeIds, withoutWmeIds :: Opts -> Opts
withWmeIds    opts = opts { optsWmeIds = True  }
withoutWmeIds opts = opts { optsWmeIds = False }

withWmeAmems, withoutWmeAmems :: Opts -> Opts
withWmeAmems    opts = opts { optsWmeAmems = True  }
withoutWmeAmems opts = opts { optsWmeAmems = False }

withWmeToks, withoutWmeToks :: Opts -> Opts
withWmeToks    opts = opts { optsWmeToks = True  }
withoutWmeToks opts = opts { optsWmeToks = False }

withWmeNegativeJoinResults, withoutWmeNegativeJoinResults :: Opts -> Opts
withWmeNegativeJoinResults    opts = opts { optsWmeNegativeJoinResults = True  }
withoutWmeNegativeJoinResults opts = opts { optsWmeNegativeJoinResults = False }

withTokIds, withoutTokIds :: Opts -> Opts
withTokIds    opts = opts { optsTokIds = True  }
withoutTokIds opts = opts { optsTokIds = False }

withTokWmes, withoutTokWmes :: Opts -> Opts
withTokWmes    opts = opts { optsTokWmes = True  }
withoutTokWmes opts = opts { optsTokWmes = False }

withTokParents, withoutTokParents :: Opts -> Opts
withTokParents    opts = opts { optsTokParents = True  }
withoutTokParents opts = opts { optsTokParents = False }

withTokNodes, withoutTokNodes :: Opts -> Opts
withTokNodes    opts = opts { optsTokNodes = True  }
withoutTokNodes opts = opts { optsTokNodes = False }

withTokChildren, withoutTokChildren :: Opts -> Opts
withTokChildren    opts = opts { optsTokChildren = True  }
withoutTokChildren opts = opts { optsTokChildren = False }

withTokJoinResults, withoutTokJoinResults :: Opts -> Opts
withTokJoinResults    opts = opts { optsTokJoinResults = True  }
withoutTokJoinResults opts = opts { optsTokJoinResults = False }

withTokNccResults, withoutTokNccResults :: Opts -> Opts
withTokNccResults    opts = opts { optsTokNccResults = True  }
withoutTokNccResults opts = opts { optsTokNccResults = False }

withTokOwners, withoutTokOwners :: Opts -> Opts
withTokOwners    opts = opts { optsTokOwners = True  }
withoutTokOwners opts = opts { optsTokOwners = False }

withAmemFields, withoutAmemFields :: Opts -> Opts
withAmemFields    opts = opts { optsAmemFields = True  }
withoutAmemFields opts = opts { optsAmemFields = False }

withAmemIds, withoutAmemIds :: Opts -> Opts
withAmemIds    opts = opts { optsAmemIds = True  }
withoutAmemIds opts = opts { optsAmemIds = False }

withAmemRefcount, withoutAmemRefcount :: Opts -> Opts
withAmemRefcount    opts = opts { optsAmemRefcount = True  }
withoutAmemRefcount opts = opts { optsAmemRefcount = False }

withAmemWmes, withoutAmemWmes :: Opts -> Opts
withAmemWmes    opts = opts { optsAmemWmes = True  }
withoutAmemWmes opts = opts { optsAmemWmes = False }

withBmemIds, withoutBmemIds :: Opts -> Opts
withBmemIds    opts = opts { optsBmemIds = True  }
withoutBmemIds opts = opts { optsBmemIds = False }

withBmemToks, withoutBmemToks :: Opts -> Opts
withBmemToks    opts = opts { optsBmemToks = True  }
withoutBmemToks opts = opts { optsBmemToks = False }

withUl, withoutUl :: Opts -> Opts
withUl    opts = opts { optsUl = True  }
withoutUl opts = opts { optsUl = False }

withJoinIds, withoutJoinIds :: Opts -> Opts
withJoinIds    opts = opts { optsJoinIds = True  }
withoutJoinIds opts = opts { optsJoinIds = False }

withJoinTests, withoutJoinTests :: Opts -> Opts
withJoinTests    opts = opts { optsJoinTests = True  }
withoutJoinTests opts = opts { optsJoinTests = False }

withJoinAmems, withoutJoinAmems :: Opts -> Opts
withJoinAmems    opts = opts { optsJoinAmems = True  }
withoutJoinAmems opts = opts { optsJoinAmems = False }

withJoinNearestAncestors, withoutJoinNearestAncestors :: Opts -> Opts
withJoinNearestAncestors    opts = opts { optsJoinNearestAncestors = True  }
withoutJoinNearestAncestors opts = opts { optsJoinNearestAncestors = False }

withNegativeIds, withoutNegativeIds :: Opts -> Opts
withNegativeIds    opts = opts { optsNegativeIds = True  }
withoutNegativeIds opts = opts { optsNegativeIds = False }

withNegativeTests, withoutNegativeTests :: Opts -> Opts
withNegativeTests    opts = opts { optsNegativeTests = True  }
withoutNegativeTests opts = opts { optsNegativeTests = False }

withNegativeAmems, withoutNegativeAmems :: Opts -> Opts
withNegativeAmems    opts = opts { optsNegativeAmems = True  }
withoutNegativeAmems opts = opts { optsNegativeAmems = False }

withNegativeNearestAncestors, withoutNegativeNearestAncestors :: Opts -> Opts
withNegativeNearestAncestors opts = opts
  { optsNegativeNearestAncestors = True  }
withoutNegativeNearestAncestors opts = opts
  { optsNegativeNearestAncestors = False }

withNegativeToks, withoutNegativeToks :: Opts -> Opts
withNegativeToks    opts = opts { optsNegativeToks = True  }
withoutNegativeToks opts = opts { optsNegativeToks = False }

withNccIds, withoutNccIds :: Opts -> Opts
withNccIds    opts = opts { optsNccIds = True  }
withoutNccIds opts = opts { optsNccIds = False }

withNccConjucts, withoutNccConjucts :: Opts -> Opts
withNccConjucts    opts = opts { optsNccConjucts = True  }
withoutNccConjucts opts = opts { optsNccConjucts = False }

withNccPartners, withoutNccPartners :: Opts -> Opts
withNccPartners    opts = opts { optsNccPartners = True  }
withoutNccPartners opts = opts { optsNccPartners = False }

withNccNodes, withoutNccNodes :: Opts -> Opts
withNccNodes    opts = opts { optsNccNodes = True  }
withoutNccNodes opts = opts { optsNccNodes = False }

withNccToks, withoutNccToks :: Opts -> Opts
withNccToks    opts = opts { optsNccToks = True  }
withoutNccToks opts = opts { optsNccToks = False }

withNccNewResultBuffers, withoutNccNewResultBuffers :: Opts -> Opts
withNccNewResultBuffers    opts = opts { optsNccNewResultBuffers = True  }
withoutNccNewResultBuffers opts = opts { optsNccNewResultBuffers = False }

withPToks, withoutPToks :: Opts -> Opts
withPToks    opts = opts { optsPToks = True  }
withoutPToks opts = opts { optsPToks = False }

withPLocations, withoutPLocations :: Opts -> Opts
withPLocations    opts = opts { optsPLocations = True  }
withoutPLocations opts = opts { optsPLocations = False }
