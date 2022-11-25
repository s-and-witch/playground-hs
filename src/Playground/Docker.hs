{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playground.Docker where

import           Control.Monad.Reader           (MonadReader (ask), ReaderT,
                                                 asks, void)
import           Data.ByteString.Lazy.Char8     (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Foldable                  (for_)
import           Playground.Files               (makeWorkspaceDir, procBS)
import           Playground.Pool                (makeWorkspacePool)
import           Playground.Types.Docker        (Docker (..), DockerEnv (..),
                                                 DockerImage (..),
                                                 DockerImagePath (..))
import           Playground.Types.GhcVersion    (GhcPath (MkGhcPath),
                                                 GhcVer (..))
import           Playground.Types.OptLevel      (OptLevel, selectOptimisation)
import           Playground.Types.Script        (Script, ScriptsDir (..),
                                                 selectScript)
import           Playground.Types.StartupConfig (StartupConfig (..))
import           Playground.Types.Timeout       (Timeout (..))
import           Playground.Types.Workspace     (RuntimeDir,
                                                 Workspace (MkWorkspace),
                                                 getVolumeName, getWorkspaceDir)
import           System.Process.Typed           (ProcessConfig,
                                                 readProcessStdout, runProcess)

runDockerCommand :: Docker -> [ByteString] -> ProcessConfig () () ()
runDockerCommand (MkDocker d) args = procBS d args

makeDockerVolume :: Docker -> Workspace -> IO ()
makeDockerVolume docker n = void do
  let
    command =
      runDockerCommand docker
        [ "volume", "create"
        , "--opt", "type=tmpfs"
        , "--opt", "device=tmpfs"
        , "--opt", "o=size=128m"
        , getVolumeName n
        ]
  runProcess command

dockerLoadImage :: Docker -> DockerImagePath -> IO DockerImage
dockerLoadImage docker (MkDockerImagePath path) = do
  let
    command =
      runDockerCommand docker
        [ "load", "-i", path ]
  (_, output) <- readProcessStdout command
  pure . MkDockerImage . last . BS.words . last . BS.lines $ output

runDocker :: Monad m => Script -> OptLevel -> GhcVer -> Workspace -> ReaderT DockerEnv m (ProcessConfig () () ())
runDocker script optLevel ghc w = do
  MkDockerEnv{..} <- ask
  let
    MkScriptsDir sd = scriptsDir

    MkDockerImage img = dockerImage

  MkGhcPath ghcPath <- askGhcPath ghc

  pure $ runDockerCommand docker
    [ "run", "-i", "--rm"
    , "--mount", "type=bind,src=" <> sd <> ",dst=/scripts,ro=1"
    , "--mount", "type=bind,src=" <> getWorkspaceDir w runtimeDir <> ",dst=/data,ro=1"
    , "--mount", "type=tmpfs,dst=/tmp"
    , "--mount", "type=volume,src=" <> getVolumeName w <> ",target=/compilation"
    , "--read-only"
    , "-m", "512MB"
    , "--cpus", "2"
    , "--network", "none"
    , "--env", "GHC=" <> ghcPath
    , img
    , "bash"
    , selectScript script, showTimeout timeout, selectOptimisation optLevel
    ]

initDockerEnv :: StartupConfig -> RuntimeDir -> IO DockerEnv
initDockerEnv MkStartupConfig{..} runtimeDir = do
  for_ [1 .. workersCount] \num -> do
    makeDockerVolume docker (MkWorkspace num)
    makeWorkspaceDir runtimeDir (MkWorkspace num)
  workspacePool <- makeWorkspacePool workersCount
  dockerImage <- dockerLoadImage docker dockerImagePath
  let ghcPaths = [ghc1Path, ghc2Path, ghc3Path, ghc4Path]
  pure MkDockerEnv{..}

askGhcPath :: Monad m => GhcVer -> ReaderT DockerEnv m GhcPath
askGhcPath GHC1 = asks (head  . ghcPaths)
askGhcPath GHC2 = asks ((!!1) . ghcPaths)
askGhcPath GHC3 = asks ((!!2) . ghcPaths)
askGhcPath GHC4 = asks ((!!3) . ghcPaths)
