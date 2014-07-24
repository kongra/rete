{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-23

module AI.Rete.WME where

import Control.Concurrent.STM

import AI.Rete.Data
import qualified AI.Rete.Symbol as Symbol

fromStrings :: Env -> String -> String -> String -> STM WME
fromStrings env obj attr val = do
  obj'  <- Symbol.intern env obj
  attr' <- Symbol.intern env attr
  val'  <- Symbol.intern env val
  return (WME obj' attr' val')

-- import qualified AI.Rete.Env as Env
-- test1 :: IO ()
-- test1 = do
--   env <-  atomically (Env.create)
--   wme1 <- atomically (fromStrings env "Ala" "ma" "psa")
--   wme2 <- atomically (fromStrings env "Ala" "ma" "kota")

--   print (wme1 == wme2)
