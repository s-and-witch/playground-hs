{-# LANGUAGE OverloadedStrings #-}

module Playground.Types.OptLevel where

import Data.ByteString.Lazy.Char8 (ByteString)

data OptLevel = O0 | O1 | O2
  deriving Show

selectOptimisation :: OptLevel -> ByteString
selectOptimisation O0 = "-O0"
selectOptimisation O1 = "-O1"
selectOptimisation O2 = "-O2"
