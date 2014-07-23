{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-23

module AI.Rete.Id (gen) where

import Control.Concurrent.STM
import AI.Rete.Data

gen :: Env -> STM ID
gen (Env ids _) = do
  modifyTVar' ids (+1)
  readTVar ids
