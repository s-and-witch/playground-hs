module Playground.Types.SessionResult where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text                  (Text)
import Data.Text                  qualified as Text
import Data.Text.Encoding         (decodeUtf8With)

newtype SessionResult = MkSessionResult { rawSessionResult :: ByteString }


prettySessionResult :: SessionResult -> Text
prettySessionResult (MkSessionResult bs)
  | BS.null bs = Text.pack "Empty output."
  | otherwise = decodeUtf8With (\ _ _ -> Nothing) . BS.toStrict $ bs
