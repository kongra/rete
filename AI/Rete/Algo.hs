{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Algo
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-07-23
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
------------------------------------------------------------------------
module AI.Rete.Algo where

import Control.Concurrent.STM
import qualified Data.HashMap.Strict as Map
-- import qualified Data.HashSet as Set

import AI.Rete.Data

-- | Sets in-transaction value of var to f applied to current
-- in-transaction-value of var. Returns the newly established
-- value. Non-strict.
alter :: TVar a -> (a -> a) -> STM a
alter var f = do
  modifyTVar var f
  readTVar var

-- | Strict version of alter.
alter' :: TVar a -> (a -> a) -> STM a
alter' var f = do
  modifyTVar' var f
  readTVar var

-- | Creates a new Env
createEnv :: STM Env
createEnv = do
  ids     <- newTVar 0
  symbols <- newTVar Map.empty
  return (Env ids symbols)

-- | Generates a new ID
genid :: Env -> STM ID
genid (Env ids _) = alter' ids (+1)

emptySymbol :: Symbol
emptySymbol = Symbol (-1) ""

emptyVariable :: Symbol
emptyVariable = Variable (-2) "?"

-- | Interns a symbol
internSymbol :: Env -> String -> STM Symbol
internSymbol _ "" = return emptySymbol

internSymbol env name@[x]
  | x == '?'  = return emptyVariable
  | otherwise = doInternSymbol env name Symbol

internSymbol env name@(x:_)
  | x == '?'  = doInternSymbol env name Variable
  | otherwise = doInternSymbol env name Symbol

type SymbolConstructor = ID -> String -> Symbol

doInternSymbol :: Env -> String -> SymbolConstructor -> STM Symbol
doInternSymbol env@(Env _ symbols) name constr = do
  registry <- readTVar symbols
  case Map.lookup name registry of
    (Just s) -> return s
    Nothing  -> do
      id' <- genid env
      let s = constr id' name
      writeTVar symbols (Map.insert name s registry)
      return s


  
    
    