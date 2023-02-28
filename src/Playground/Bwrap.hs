{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playground.Bwrap where

import           Control.Monad.Reader           (MonadReader (ask), ReaderT,
                                                 asks)
import           Data.ByteString.Lazy.Char8     (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Playground.Files               (procBS, mkFilePath)
import           Playground.Types.Bwrap         (BwrapEnv (..))
import           Playground.Types.GhcVersion    (GhcPath (MkGhcPath),
                                                 GhcVer (..))
import           Playground.Types.OptLevel      (OptLevel, selectOptimisation)
import           Playground.Types.Script        (Script, selectScript)
import           Playground.Types.StartupConfig (StartupConfig (..))
import           Playground.Types.Timeout       (Timeout (..))
import           System.Process.Typed           (ProcessConfig)


runBwrap :: Monad m => Script -> OptLevel -> GhcVer -> ByteString ->  ReaderT BwrapEnv m (ProcessConfig () () ())
runBwrap script optLevel ghc workspaceDir = do
  MkBwrapEnv{..} <- ask

  MkGhcPath ghcPath <- askGhcPath ghc

  pure $ procBS "systemd-run" $
    [ "--user", "--scope", "-q"
    , "-p", "MemoryAccounting=yes"
    , "-p", "MemoryMax=512M"
    , "--"
    , bwrap, "--unshare-all"
    , "--size", intToBs (128 * 1024 * 1024), "--tmpfs", "/"
    , "--dev", "/dev"
    , "--proc", "/proc"
    , "--ro-bind", scriptsDir, "/scripts"
    , "--ro-bind", workspaceDir, "/data"
    ] <> concatMap makeRoBinds runtimeDeps <>
    [ "--clearenv"
    , "--setenv", "PATH", makePath runtimeDeps
    , "--setenv", "GHC", ghcPath
    , "--"
    , "bash"
    , selectScript script, showTimeout timeout, selectOptimisation optLevel
    ]
  where
    intToBs :: Integer -> ByteString
    intToBs = BS.pack . show

makeRoBinds :: ByteString -> [ByteString]
makeRoBinds path = ["--ro-bind", path, path]

makePath :: [ByteString] -> ByteString
makePath = BS.intercalate ":" . map (<> "/bin")

initBwrapEnv :: StartupConfig -> IO BwrapEnv
initBwrapEnv MkStartupConfig{..} = do
  let ghcPaths = [ghc1Path, ghc2Path, ghc3Path, ghc4Path]
  runtimeDeps <- BS.lines <$> BS.readFile (mkFilePath ghcDeps)
  pure MkBwrapEnv{..}

askGhcPath :: Monad m => GhcVer -> ReaderT BwrapEnv m GhcPath
askGhcPath GHC1 = asks (head  . ghcPaths)
askGhcPath GHC2 = asks ((!!1) . ghcPaths)
askGhcPath GHC3 = asks ((!!2) . ghcPaths)
askGhcPath GHC4 = asks ((!!3) . ghcPaths)
