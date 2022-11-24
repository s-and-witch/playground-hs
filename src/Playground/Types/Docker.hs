{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Playground.Types.Docker where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Pool                  (Pool)
import           Playground.Types.Script    (ScriptsDir)
import           Playground.Types.Workspace (RuntimeDir, Workspace)
import           System.Envy                (Var)


newtype Docker = MkDocker ByteString
  deriving Show
  deriving newtype Var

newtype DockerImagePath = MkDockerImagePath ByteString
  deriving Show
  deriving newtype Var

newtype DockerImage = MkDockerImage ByteString
  deriving Show

data DockerEnv = MkDockerEnv
  { dockerImage   :: DockerImage
  , scriptsDir    :: ScriptsDir
  , workspacePool :: Pool Workspace
  , runtimeDir    :: RuntimeDir
  , docker        :: Docker
  }
