{-# LANGUAGE Trustworthy           #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Test
-- Copyright   : (c) 2015 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2015-03-20
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Test where

import AI.Rete
import AI.Rete.Print

test1 :: IO ()
test1 =
  execAndPrint switch
  [
    addWme "sójka" "jestPtak" True

  , addProd    [c      (var  "x"     ) "jestPtak"    True] []
    $ \actx -> [addWme (valE "x" actx) "jestZwierzę" True]

  , addProd [c (var "x") "jestZwierzę" True] [] (traceTokAction "OK1: ")
  ]
  where switch = withNet . withData
