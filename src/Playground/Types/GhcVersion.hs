{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Playground.Types.GhcVersion where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           System.Envy

data GhcVer = GHC1 | GHC2 | GHC3 | GHC4
  deriving Show

newtype GhcPath = MkGhcPath { selectPath :: ByteString }
  deriving Show
  deriving newtype Var
