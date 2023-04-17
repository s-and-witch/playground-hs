{-# LANGUAGE DerivingStrategies         #-}

module Playground.Types.GhcVersion where

import Data.ByteString.Lazy.Char8 (ByteString)

import System.Envy

newtype GhcPath = MkGhcPath { selectPath :: ByteString }
  deriving Show
  deriving newtype Var
