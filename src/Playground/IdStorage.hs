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

lookupStorage' :: MessageId -> ChatId -> AcidState IdStorage -> IO (Maybe (Int, UTCTime))
lookupStorage' (MessageId userMessageId) (ChatId chatId) is =
  query is (LookupKey (fromInteger userMessageId, fromInteger chatId))

lookupStorage :: MessageId -> ChatId -> AcidState IdStorage -> IO (Maybe MessageId)
lookupStorage userId chatId is =
  fmap (MessageId . fromIntegral . fst) <$> lookupStorage' userId chatId is

insertStorage' :: MessageId -> ChatId -> MessageId -> UTCTime -> AcidState IdStorage -> IO ()
insertStorage' (MessageId userMessageId) (ChatId chatId) (MessageId botMessageId) time is =
  update is (InsertKey
    (fromInteger userMessageId, fromInteger chatId)
    (fromInteger botMessageId, time))

insertStorage :: MessageId -> ChatId -> MessageId -> AcidState IdStorage -> IO ()
insertStorage userId chatId botId is = do
  time <- getCurrentTime
  insertStorage' userId chatId botId time is

updateStorage :: MessageId -> ChatId -> MessageId -> AcidState IdStorage -> IO ()
updateStorage userId chatId botId is = do
  mval <- lookupStorage' userId chatId is
  case mval of
    Nothing -> insertStorage userId chatId botId is
    Just (_, time) -> insertStorage' userId chatId botId time is
