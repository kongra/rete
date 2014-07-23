{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-21

module AI.Rete.Env (create) where

import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

import AI.Rete.Data

create :: STM Env
create = do
  ids     <- newTVar regularIDStart
  symbols <- newTVar Map.empty
  return (Env ids symbols)
