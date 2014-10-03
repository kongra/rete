{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Net
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-03
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- This module contains routines for managing the Rete network
-- structure.
------------------------------------------------------------------------
module AI.Rete.Net where
