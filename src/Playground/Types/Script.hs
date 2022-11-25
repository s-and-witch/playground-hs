{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Playground.Types.Script where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           System.Envy                (Var)

data Script = Run | Core
  deriving Show

selectScript :: Script -> ByteString
selectScript Run  = "scripts/run"
selectScript Core = "scripts/core"

newtype ScriptsDir = MkScriptsDir ByteString
  deriving Show
  deriving newtype Var
