module Playground.Files where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS

import System.Process.Typed       (ProcessConfig, proc)

import UnliftIO


mkFilePath :: ByteString -> FilePath
mkFilePath = BS.unpack

procBS :: ByteString -> [ByteString] -> ProcessConfig () () ()
procBS c args = proc (BS.unpack c) (map BS.unpack args)

withWorkspaceDir :: MonadUnliftIO m => (ByteString -> m a) -> m a
withWorkspaceDir callback =  withSystemTempDirectory "playdroundhs" (callback . BS.pack)
