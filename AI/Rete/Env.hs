-- Copyright : (c) Konrad Grzanek 2014
-- Created   : 2014-07-21

{- LANGUAGE Safe -}
{-# OPTIONS_GHC -Wall #-}

module AI.Rete.Env where

import Control.Concurrent.STM

type ID = Int
type IDS = TVar ID
data Env = Env { ids :: IDS }

-- Env CREATION
create :: STM Env
create = do
  newIds <- newTVar 0
  return (Env newIds)

-- GENERATING IDS
genid :: Env -> STM ID
genid env = do
  let idsv = ids env
  modifyTVar' idsv (+1)
  readTVar idsv

-- test1 :: IO [ID]
-- test1 = do
--   atomically $ do
--     env <- create
--     id1 <- genid env
--     id2 <- genid env
--     id3 <- genid env
    
--     return [id1, id2, id3]
    
-- test2 :: IO ()
-- test2 = do
--   env <- atomically create
--   id1 <- atomically $ genid env
--   id2 <- atomically $ genid env
--   id3 <- atomically $ genid env
  
--   print id1
--   print id2
--   print id3