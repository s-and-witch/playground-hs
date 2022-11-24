module Playground.Files where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Playground.Types.Workspace (RuntimeDir (MkRuntimeDir),
                                             Workspace, getWorkspaceDir)
import           System.Directory           (createDirectory)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process.Typed       (ProcessConfig, proc)


mkFilePath :: ByteString -> FilePath
mkFilePath = BS.unpack

procBS :: ByteString -> [ByteString] -> ProcessConfig () () ()
procBS c args = proc (BS.unpack c) (map BS.unpack args)

withPlaygroundRuntimeDir :: (RuntimeDir -> IO a) -> IO a
withPlaygroundRuntimeDir callback =
  withSystemTempDirectory "playdroundhs" (callback . MkRuntimeDir . BS.pack)


makeWorkspaceDir :: RuntimeDir -> Workspace -> IO ()
makeWorkspaceDir rd w = do
  let dir = getWorkspaceDir w rd
  createDirectory (mkFilePath dir)
