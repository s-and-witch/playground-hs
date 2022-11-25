module Playground.Types.SessionConfig where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Playground.Types.GhcVersion (GhcVer)
import           Playground.Types.OptLevel   (OptLevel)
import           Playground.Types.Script     (Script)


data SessionConfig = MkSessionConfig
  { script     :: Script
  , optLevel   :: OptLevel
  , ghcVersion :: GhcVer
  , content    :: ByteString
  }
  deriving Show
