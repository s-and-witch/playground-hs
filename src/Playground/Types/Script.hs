{-# LANGUAGE OverloadedStrings          #-}

module Playground.Types.Script where

import Data.ByteString.Lazy.Char8 (ByteString)

data Script = Run | Core
  deriving Show

selectScript :: Script -> ByteString
selectScript Run  = "scripts/run"
selectScript Core = "scripts/core"
