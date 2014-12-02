{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Test1
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-11-27
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- Test module for Rete.
------------------------------------------------------------------------

module AI.Rete.Test1 where

import AI.Rete
import AI.Rete.Data
import AI.Rete.Print
import Control.Concurrent.STM

passAction :: Action
passAction _ = return ()

test1 :: IO ()
test1 = do
  env  <- atomically createEnv
  node <- atomically (addProduction
                      env
                      [
                        c "?x" "on" "?y"
                        -- , c   "?y" "left-of" "?z"
                        -- , c   "?z" "color"   "red"
                        ,
                        neg "?z" "on" "?x"
                      ]
                      passAction
                      Nothing)

  s1 <- atomically $ toString boundless soleNetBottomUp node
  putStrLn s1
  let dtm = envDummyTopNode env
  s2 <- atomically $ toString boundless soleNetTopDown dtm
  putStrLn s2

  -- status <- atomically $ removeProduction env node
  -- putStrLn (show status)

  -- s3 <- atomically $ toString boundless soleNetTopDown dtm
  -- putStrLn s3

  return ()
