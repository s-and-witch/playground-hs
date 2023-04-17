{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Playground.Bwrap where

import Control.Monad.Reader           (MonadReader (ask), ReaderT)

import Data.ByteString.Lazy.Char8     (ByteString)
import Data.ByteString.Lazy.Char8     qualified as BS

import Playground.Files               (mkFilePath, procBS)
import Playground.Types.Bwrap         (BwrapEnv (..))
import Playground.Types.GhcVersion    (GhcPath (..))
import Playground.Types.OptLevel      (OptLevel, selectOptimisation)
import Playground.Types.Script        (Script, selectScript)
import Playground.Types.StartupConfig (StartupConfig (..))
import Playground.Types.Timeout       (Timeout (..))

import System.Process.Typed           (ProcessConfig)


runBwrap :: Monad m => Script -> OptLevel -> GhcPath -> ByteString ->  ReaderT BwrapEnv m (ProcessConfig () () ())
runBwrap script optLevel (MkGhcPath ghcPath) workspaceDir = do
  MkBwrapEnv{..} <- ask

  pure $ procBS "systemd-run" $
    [ "--scope", "-q"
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
  runtimeDeps <- BS.lines <$> BS.readFile (mkFilePath ghcDeps)
  pure MkBwrapEnv{..}
