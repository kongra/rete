{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-23

module AI.Rete.Symbol (intern) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

import AI.Rete.Data
import qualified AI.Rete.Id as Id

emptySymbol :: Symbol
emptySymbol = Symbol 0 ""

emptyVariable :: Symbol
emptyVariable = Variable 0 "?"

intern :: Env -> String -> STM Symbol
intern _ "" = return emptySymbol

intern env name@[x]
  | x == '?'  = return emptyVariable
  | otherwise = doIntern env name Symbol

intern env name@(x:_)
  | x == '?'  = doIntern env name Variable
  | otherwise = doIntern env name Symbol

type SymbolConstructor = ID -> String -> Symbol

doIntern :: Env -> String -> SymbolConstructor -> STM Symbol
doIntern env@(Env _ symbols) name constr = do
  registry <- readTVar symbols
  case Map.lookup name registry of
    (Just s) -> return s
    Nothing  -> do
      id' <- Id.gen env
      let s = constr id' name
      writeTVar symbols (Map.insert name s registry)
      return s
