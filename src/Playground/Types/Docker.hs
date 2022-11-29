{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Playground.Types.Docker where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Playground.Types.GhcVersion (GhcPath)
import           Playground.Types.Timeout    (Timeout)
import           System.Envy                 (Var)


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
  , ghcPaths      :: [GhcPath]
  , scriptsDir    :: ByteString
  , workspaceDir  :: ByteString
  , volumeName    :: ByteString
  , docker        :: Docker
  , timeout       :: Timeout
  }
