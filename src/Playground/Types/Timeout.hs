{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Playground.Types.Timeout where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS

import System.Envy                (Var (..))


newtype Timeout = MkTimeout { showTimeout :: ByteString }
  deriving (Show)

instance Var Timeout where
  toVar (MkTimeout bs) = toVar . BS.intercalate "," . BS.split ' ' $ bs
  fromVar =  (fmap . fmap) (MkTimeout . BS.intercalate " " . BS.split ',') fromVar
