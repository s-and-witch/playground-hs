module Playground.Types.StartupConfig where

import           Data.Text                   (Text)
import           Playground.Types.Docker     (Docker, DockerImagePath)
import           Playground.Types.GhcVersion (GhcPath)
import           Playground.Types.Script     (ScriptsDir)
import           Playground.Types.Timeout
import           System.Envy                 (FromEnv (fromEnv), env)


data StartupConfig = MkStartupConfig
  { workersCount    :: Int
  , scriptsDir      :: ScriptsDir
  , ghc1Path        :: GhcPath
  , ghc2Path        :: GhcPath
  , ghc3Path        :: GhcPath
  , ghc4Path        :: GhcPath
  , dockerImagePath :: DockerImagePath
  , docker          :: Docker
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
    <*> env "DOCKER_IMAGE"
    <*> env "DOCKER"
    <*> env "TG_TOKEN"
    <*> env "TIMEOUT"
