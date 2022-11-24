{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Playground.Types.Workspace where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Coerce                (coerce)


newtype Workspace = MkWorkspace Int
  deriving newtype (Show, Num, Enum)

newtype RuntimeDir = MkRuntimeDir ByteString

workspaceNumber :: Workspace -> Int
workspaceNumber = coerce

getVolumeName :: Workspace -> ByteString
getVolumeName n = "playgroundhs" <> BS.pack (show n)

getWorkspaceDir :: Workspace -> RuntimeDir -> ByteString
getWorkspaceDir n (MkRuntimeDir rd) = rd <> "/" <> BS.pack (show n)
