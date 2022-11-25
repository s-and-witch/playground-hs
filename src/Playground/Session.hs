{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playground.Session where

import           Control.Monad.Reader           (MonadIO (liftIO), ReaderT,
                                                 asks)
import           Data.ByteString.Lazy.Char8     (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Playground.Docker              (runDocker)
import           Playground.Files               (mkFilePath)
import           Playground.Pool                (withResource')
import           Playground.Types.Docker        (DockerEnv (runtimeDir, workspacePool))
import           Playground.Types.SessionConfig (SessionConfig (..))
import           Playground.Types.Workspace     (getWorkspaceDir)
import           System.Directory               (removeFile)
import           System.Process.Typed           (ExitCode (..),
                                                 readProcessStderr)


runPlaygroundSession :: SessionConfig -> ReaderT DockerEnv IO ByteString
runPlaygroundSession MkSessionConfig{..} = do
  wp <- asks workspacePool
  rd <- asks runtimeDir
  withResource' wp \w -> do

    let
      dir = getWorkspaceDir w rd
      filename = dir <> "/Main.hs"

    liftIO $ BS.writeFile (mkFilePath filename) content

    command <- runDocker script optLevel ghcVersion w
    (errCode, output) <- readProcessStderr command

    liftIO $ removeFile (mkFilePath filename)

    case errCode of
      ExitSuccess -> pure output
      ExitFailure 137 -> pure "Your code was killed with SIGKILL (probably OOM?)"
      ExitFailure 124 -> pure "Your code was finished with 124 exit code (probably timeout?)"
      ExitFailure n   -> pure $ output <> "\nExit code: " <> BS.pack (show n)
    -- Todo: retrun ADT here
