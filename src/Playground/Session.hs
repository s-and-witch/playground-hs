{-# LANGUAGE OverloadedStrings #-}

module Playground.Session where

import Control.Monad.Reader           (MonadIO (liftIO), ReaderT)

import Data.ByteString.Lazy.Char8     qualified as BS

import Playground.Bwrap               (runBwrap)
import Playground.Files               (mkFilePath, withWorkspaceDir)
import Playground.Types.Bwrap         (BwrapEnv)
import Playground.Types.SessionConfig (SessionConfig (..))
import Playground.Types.SessionResult (SessionResult (MkSessionResult))

import System.Directory               (removeFile)
import System.Process.Typed           (ExitCode (..), readProcessStderr)
import Data.Char (isSpace)


runPlaygroundSession :: SessionConfig -> ReaderT BwrapEnv IO SessionResult
runPlaygroundSession MkSessionConfig{..}
  | not (BS.null content)
  , not (BS.all isSpace content) = withWorkspaceDir \workspaceDir -> do

  let filename = workspaceDir <> "/Main.hs"

  liftIO $ BS.writeFile (mkFilePath filename) content

  command <- runBwrap script optLevel ghcPath workspaceDir
  (errCode, output) <- readProcessStderr command

  liftIO $ removeFile (mkFilePath filename)

  -- Todo: retrun ADT here
  pure $ MkSessionResult case errCode of
    ExitSuccess     -> output
    ExitFailure 137 -> "Your code was killed with SIGKILL (probably OOM?)"
    ExitFailure 124 -> "Your code was finished with 124 exit code (probably timeout?)"
    ExitFailure n   -> output <> "\nExit code: " <> BS.pack (show n)
  | otherwise = pure (MkSessionResult "Please, edit your message to give me some haskell code")
