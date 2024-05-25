{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Playground.Types.Timeout where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS

import System.Envy                (Var (..))
import Data.List.Split
import Text.Read


data Timeout = MkTimeout { ghcTerm :: Int, ghcKill :: Int, processTerm :: Int, processKill :: Int }
  deriving (Show)

showTimeout :: Timeout -> ByteString
showTimeout MkTimeout{..} = BS.unwords [showBS ghcTerm, showBS ghcKill, showBS processTerm, showBS processKill]
  where showBS = BS.pack . show

instance Var Timeout where
  toVar = toVar . BS.intercalate "," . BS.split ' ' . showTimeout
  fromVar str = do
    [ghcTermBS, ghcKillBS, processTermBS, processKillBS] <- pure (splitWhen  (== ',') str)
    ghcTerm <- readMaybe ghcTermBS
    ghcKill <- readMaybe ghcKillBS
    processTerm <- readMaybe processTermBS
    processKill <- readMaybe processKillBS
    pure MkTimeout{..}
