{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-23

module AI.Rete.Data where

import qualified Data.Map.Strict as Map
import Control.Concurrent.STM (TVar)

-- IDENTIFIERS
type ID = Int

regularIDStart :: ID
regularIDStart = 999 -- First 999 are reserved

-- ENVIRONMENT
data Env = Env
           {-# UNPACK #-} !(TVar ID)             -- id
           {-# UNPACK #-} !(TVar SymbolRegistry) -- symbols

-- SYMBOLS (INCLUDING VARIABLES)
data Symbol = Symbol   {-# UNPACK #-} !ID !String
            | Variable {-# UNPACK #-} !ID !String

type SymbolRegistry = Map.Map String Symbol

instance Show Symbol where
  show (Symbol   _ s) = s
  show (Variable _ s) = s

instance Eq Symbol where
  (Symbol   id1 _) == (Symbol   id2 _) = id1 == id2
  (Variable id1 _) == (Variable id2 _) = id1 == id2
  _ == _ = False

-- WMES 
data WME = WME 
           !Symbol -- obj 
           !Symbol -- attr
           !Symbol -- val

instance Show WME where
  show (WME obj attr val) = "(" ++
                            show obj  ++ "," ++
                            show attr ++ "," ++
                            show val  ++
                            ")"

instance Eq WME where
  (WME obj1 attr1 val1) == (WME obj2 attr2 val2) =
    obj1  == obj2  &&
    attr1 == attr2 &&
    val1  == val2
