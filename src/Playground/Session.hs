{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playground.Session where

import           Control.Monad.Reader           (MonadIO (liftIO), ReaderT,
                                                 asks)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Playground.Docker              (runDocker)
import           Playground.Files               (mkFilePath)
import           Playground.Types.Docker        (DockerEnv (workspaceDir))
import           Playground.Types.SessionConfig (SessionConfig (..))
import           Playground.Types.SessionResult (SessionResult (MkSessionResult))
import           System.Directory               (removeFile)
import           System.Process.Typed           (ExitCode (..),
                                                 readProcessStderr)


runPlaygroundSession :: SessionConfig -> ReaderT DockerEnv IO SessionResult
runPlaygroundSession MkSessionConfig{..} = do
  rd <- asks workspaceDir

  let filename = rd <> "/Main.hs"

  liftIO $ BS.writeFile (mkFilePath filename) content

  command <- runDocker script optLevel ghcVersion
  (errCode, output) <- readProcessStderr command

  liftIO $ removeFile (mkFilePath filename)

  pure $ MkSessionResult case errCode of
    ExitSuccess -> output
    ExitFailure 137 -> "Your code was killed with SIGKILL (probably OOM?)"
    ExitFailure 124 -> "Your code was finished with 124 exit code (probably timeout?)"
    ExitFailure n   -> output <> "\nExit code: " <> BS.pack (show n)
  -- Todo: retrun ADT here
