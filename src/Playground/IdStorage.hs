module Playground.IdStorage where
import Data.Acid
import Data.Time

import Playground.Types.IdStorage

import System.Directory           (createDirectoryIfMissing)
import System.FilePath

import Telegram.Bot.API


initStorage :: FilePath -> IO (AcidState IdStorage)
initStorage fp = do
  let storageDir = fp </> "id-storage"
  createDirectoryIfMissing True storageDir
  is <- openLocalStateFrom storageDir emptyStorage
  time <- subtractOneDay <$> getCurrentTime
  update is (RemoveOlderThen time)
  createCheckpoint is
  pure is

subtractOneDay :: UTCTime -> UTCTime
subtractOneDay t = addUTCTime (negate oneDay) t where
  oneDay = secondsToNominalDiffTime (60 * 60 * 24)

lookupStorage :: MessageId -> ChatId -> AcidState IdStorage -> IO (Maybe MessageId)
lookupStorage (MessageId userMessageId) (ChatId chatId) is = do
  mval <- query is (LookupKey (fromInteger userMessageId, fromInteger chatId))
  case mval of
    Nothing       -> pure Nothing
    Just (val, _) -> pure (Just (MessageId (fromIntegral val)))

inserStorage :: MessageId -> ChatId -> MessageId -> AcidState IdStorage -> IO ()
inserStorage (MessageId userMessageId) (ChatId chatId) (MessageId botMessageId) is = do
  time <- getCurrentTime
  update is (InsertKey
    (fromInteger userMessageId, fromInteger chatId)
    (fromInteger botMessageId, time))
