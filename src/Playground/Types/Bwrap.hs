module Playground.Types.Bwrap where

import Data.ByteString.Lazy.Char8 (ByteString)

import Playground.Types.Timeout   (Timeout)


type Bwrap = ByteString

data BwrapEnv = MkBwrapEnv
  { scriptsDir  :: ByteString
  , bwrap       :: Bwrap
  , runtimeDeps :: [ByteString]
  , timeout     :: Timeout
  }
