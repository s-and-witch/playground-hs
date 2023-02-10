module Playground.Types.SessionResult where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8With)

newtype SessionResult = MkSessionResult { rawSessionResult :: ByteString }


prettySessionResult :: SessionResult -> Text
prettySessionResult = decodeUtf8With (\ _ _ -> Just '🤔') . BS.toStrict . rawSessionResult
