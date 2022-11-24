module Playground.Types.StartupConfig where

import           Data.Text               (Text)
import           Playground.Types.Docker (Docker, DockerImagePath)
import           Playground.Types.Script (ScriptsDir)
import           System.Envy             (FromEnv (fromEnv), env)


data StartupConfig = MkStartupConfig
  { workersCount :: Int
  , scriptsDir   :: ScriptsDir
  , ghc902Image  :: DockerImagePath
  , docker       :: Docker
  , tgToken      :: Text
  }
  deriving Show

instance FromEnv StartupConfig where
  fromEnv _ = MkStartupConfig
    <$> env "WORKERS_COUNT"
    <*> env "SCRIPTS_DIR"
    <*> env "GHC902_IMAGE"
    <*> env "DOCKER"
    <*> env "TG_TOKEN"
