{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Playground.Types.IdStorage where

import Control.Monad.Reader
import Control.Monad.State

import Data.Acid
import Data.Map.Strict      qualified as Map
import Data.SafeCopy
import Data.Time
import Data.Typeable

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = (Int, Int)
type Value = (Int, UTCTime)

data IdStorage = IdStorage !(Map.Map Key Value)
    deriving (Typeable)

emptyStorage :: IdStorage
emptyStorage = IdStorage Map.empty

$(deriveSafeCopy 0 'base ''IdStorage)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey :: Key -> Value -> Update IdStorage ()
insertKey key value = do
  IdStorage m <- get
  put (IdStorage (Map.insert key value m))

lookupKey :: Key -> Query IdStorage (Maybe Value)
lookupKey key = do
  IdStorage m <- ask
  return (Map.lookup key m)

removeOlderThen :: UTCTime -> Update IdStorage ()
removeOlderThen time = do
  IdStorage m <- get
  let m' = Map.filter (\(_, t) -> t < time) m
  put (IdStorage m')

$(makeAcidic ''IdStorage ['insertKey, 'lookupKey, 'removeOlderThen])
