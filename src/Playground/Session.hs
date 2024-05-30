{-# LANGUAGE OverloadedStrings #-}

module Playground.Session where

import Control.Monad.Reader           (MonadIO (liftIO), ReaderT, asks)

import Data.ByteString.Lazy.Char8     qualified as BS
import Data.Char                      (isSpace)

import Playground.Bwrap               (runBwrap)
import Playground.Files               (mkFilePath, withWorkspaceDir)
import Playground.Types.Bwrap         (BwrapEnv (..))
import Playground.Types.SessionConfig (SessionConfig (..))
import Playground.Types.SessionResult (SessionResult (MkSessionResult))
import Playground.Types.Timeout       (Timeout (..))

import System.Directory               (removeFile)
import System.Process.Typed           (ExitCode (..), readProcessStderr)

import UnliftIO.Timeout


runPlaygroundSession :: SessionConfig -> ReaderT BwrapEnv IO SessionResult
runPlaygroundSession MkSessionConfig{..}
  | not (BS.null content)
  , not (BS.all isSpace content) = withWorkspaceDir \workspaceDir -> do

  let filename = workspaceDir <> "/Main.hs"

  liftIO $ BS.writeFile (mkFilePath filename) content

  timeoutConf <- asks (.timeout)

  command <- runBwrap script optLevel ghcPath workspaceDir
  res <-
    timeout ((timeoutConf.ghcKill + timeoutConf.processKill) * 1000 * 1000) $
    readProcessStderr command

  liftIO $ removeFile (mkFilePath filename)

  -- Todo: retrun ADT here
  pure $ MkSessionResult case res of
    Nothing -> "Your code was timeouted"
    Just (errCode, output) -> case errCode of
      ExitSuccess     -> output
      ExitFailure 137 -> "Your code was killed with SIGKILL (probably OOM?)"
      ExitFailure 124 -> "Your code was finished with 124 exit code (probably timeout?)"
      ExitFailure n   -> output <> "\nExit code: " <> BS.pack (show n)
  | otherwise = pure (MkSessionResult "Please, edit your message to give me some haskell code")
