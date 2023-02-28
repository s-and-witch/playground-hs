{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Playground.Types.Bwrap where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Playground.Types.GhcVersion (GhcPath)
import           Playground.Types.Timeout    (Timeout)


type Bwrap = ByteString

data BwrapEnv = MkBwrapEnv
  { ghcPaths      :: [GhcPath]
  , scriptsDir    :: ByteString
  , bwrap         :: Bwrap
  , runtimeDeps   :: [ByteString]
  , timeout       :: Timeout
  }
