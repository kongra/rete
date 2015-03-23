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
import AI.Rete.Data
import AI.Rete.Print
import Control.Monad (forM_)
import Data.List (permutations)

mkConstant :: String -> Int -> ConstantOrVariable
mkConstant s i = JustConstant
                 (NamedPrimitiveConstant
                  (NamedPrimitive
                   (IntPrimitive i) s))

mkVariable :: String -> Int -> ConstantOrVariable
mkVariable s i = JustVariable
                 (NamedPrimitiveVariable
                  (NamedPrimitive
                   (IntPrimitive i) s))

constants :: [ConstantOrVariable]
constants =  [ mkConstant "a" 1
             , mkConstant "b" 2
             , mkConstant "c" 3]

variables :: [ConstantOrVariable]
variables =  [ mkVariable "x" 4
             , mkVariable "y" 5
             , mkVariable "z" 6 ]

type Tuple3 a = (a, a, a, a, a, a, a, a, a)

p3Constants :: [Tuple3 ConstantOrVariable]
p3Constants = [( s1, s2, s3
               , s4, s5, s6
               , s7, s8, s9)
               |
                 s1 <- constants
               , s2 <- constants
               , s3 <- constants
               , s4 <- constants
               , s5 <- constants
               , s6 <- constants
               , s7 <- constants
               , s8 <- constants
               , s9 <- constants ]

justConst :: ConstantOrVariable -> Constant
justConst (JustConstant c') = c'
justConst (JustVariable _ ) = error "NO!"

test3Prod :: Tuple3 ConstantOrVariable -> IO ()
test3Prod t = do
  let (s1, s2, s3, s4, s5, s6 , s7, s8, s9) = t
      agenda = [ addWme (justConst s1) (justConst s2) (justConst s3)
               , addWme (justConst s4) (justConst s5) (justConst s6)
               , addWme (justConst s7) (justConst s8) (justConst s9)
               , addWme (justConst s7) (justConst s8) "aaa"

               , addProd [ c s1 s2 s3
                         , c s4 s5 s6
                         , c s7 s8 s9 ] [] (traceTokAction "tok: ") ]

  putStrLn (show t)
  forM_ (permutations agenda) $ \a -> do
    _ <- execIO breadthFirst a emptyRete
    return ()

  return ()

test3 :: IO ()
test3 = mapM_ test3Prod (take 2 p3Constants)

test4 :: IO ()
test4 = do
  let agenda =
        [
          addProd [ c (var "p") "jest ptakiem" True
                  , c (var "p") "ma kolor"     "szary"] []
          passAction
          -- $
          -- traceAction $ \actx -> do
          --   p <- valE "p" actx
          --   return ("oto wróbel " ++ show p)

        , addWme "ptica"  "jest ptakiem" True
        , addWme "ptica"  "ma kolor"     "szary"
        , addWme "kotica" "ma kolor"     "szary"
        ]

  _ <- execIO breadthFirst agenda emptyRete
  return ()
  -- forM_ (permutations agenda) $ \a ->
  --   execIO breadthFirst a emptyRete

  -- putStrLn "Done"
