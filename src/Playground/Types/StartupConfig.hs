module Playground.Types.StartupConfig where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.Text                   (Text)
import           Playground.Types.Bwrap      (Bwrap)
import           Playground.Types.GhcVersion (GhcPath)
import           Playground.Types.Timeout    (Timeout)
import           System.Envy                 (FromEnv (fromEnv), env)


data StartupConfig = MkStartupConfig
  { workersCount    :: Int
  , scriptsDir      :: ByteString
  , ghc1Path        :: GhcPath
  , ghc2Path        :: GhcPath
  , ghc3Path        :: GhcPath
  , ghc4Path        :: GhcPath
  , bwrap           :: Bwrap
  , ghcDeps         :: ByteString
  , tgToken         :: Text
  , timeout         :: Timeout
  }
  deriving Show

instance FromEnv StartupConfig where
  fromEnv _ = MkStartupConfig
    <$> env "WORKERS_COUNT"
    <*> env "SCRIPTS_DIR"
    <*> env "GHC1"
    <*> env "GHC2"
    <*> env "GHC3"
    <*> env "GHC4"
    <*> env "BWRAP"
    <*> env "GHC_DEPS"
    <*> env "TG_TOKEN"
    <*> env "TIMEOUT"
