module Playground.Types.StartupConfig where

import Data.ByteString.Lazy.Char8  (ByteString)
import Data.ByteString.Lazy.Char8  qualified as BS8
import Data.Map.Strict             qualified as Map
import Data.Text                   (Text)
import Data.Traversable            (for)

import Playground.Types.Bwrap      (Bwrap)
import Playground.Types.GhcVersion (GhcPath)
import Playground.Types.Timeout    (Timeout)

import System.Envy                 (FromEnv (fromEnv), Parser, env)


data StartupConfig = MkStartupConfig
  { workersCount :: Int
  , scriptsDir   :: ByteString
  , ghcMap       :: Map.Map ByteString GhcPath
  , ghcDefault   :: GhcPath
  , workDir      :: FilePath
  , bwrap        :: Bwrap
  , ghcDeps      :: ByteString
  , tgToken      :: Text
  , timeout      :: Timeout
  , timeoutProg  :: ByteString
  }
  deriving Show

instance FromEnv StartupConfig where
  fromEnv _ = do
    workersCount <- env "WORKERS_COUNT"
    scriptsDir   <- env "SCRIPTS_DIR"
    ghcs         <- BS8.split ',' <$> env "GHCS"
    ghcMap       <- loadGhcs ghcs
    ghcDefault   <- env "DEFAULT_GHC"
    workDir      <- env "WORK_DIR"
    bwrap        <- env "BWRAP"
    ghcDeps      <- env "GHC_DEPS"
    tgToken      <- env "TG_TOKEN"
    timeout      <- env "TIMEOUT"
    timeoutProg  <- env "TIMEOUT_PROG"
    pure MkStartupConfig {..}

loadGhcs :: [ByteString] -> Parser (Map.Map ByteString GhcPath)
loadGhcs ghcs = Map.fromList <$> for ghcs \ghc -> do
  let ghc' = BS8.unpack ghc
  ghcPath <- env ghc'
  pure (ghc, ghcPath)
