{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Applicative              (optional)
import Control.Concurrent
import Control.Concurrent.STM           (atomically)
import Control.Exception
import Control.Monad.IO.Class           (MonadIO (liftIO))

import Data.Acid                        (AcidState)
import Data.ByteString.Lazy.Char8       qualified as BS
import Data.List                        (find)
import Data.Map.Strict                  qualified as Map
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.Text.Encoding               (encodeUtf8)

import Playground.Bwrap                 (initBwrapEnv)
import Playground.IdStorage
import Playground.TaskQueue             (newTaskQueue, pushTask)
import Playground.Types.GhcVersion
import Playground.Types.IdStorage
import Playground.Types.OptLevel        (OptLevel (..))
import Playground.Types.Script          (Script (..))
import Playground.Types.SessionConfig   (SessionConfig (MkSessionConfig))
import Playground.Types.SessionResult   (SessionResult, prettySessionResult)
import Playground.Types.StartupConfig   (StartupConfig (..))
import Playground.Types.TaskQueue       (TaskQueue)
import Playground.Utils                 (raceAll_)
import Playground.Worker                (runWorker)

import System.Envy                      (decodeEnv)

import Telegram.Bot.API
import Telegram.Bot.Simple hiding (editMessageText)
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Control.Monad (void)

main :: IO ()
main = do
  e <- decodeEnv
  case e of
    Left err -> putStrLn err
    Right startupConf@MkStartupConfig{tgToken} -> mdo
      idStorage <- initStorage startupConf.workDir
      bwrapEnvs <- initBwrapEnv startupConf
      putStrLn "Bwrap env initialized!"
      tq <- newTaskQueue 100 cbWrapped
      cb <- runTg idStorage startupConf.ghcDefault (startupConf.ghcMap Map.!?) tgToken tq
      cbWrapped <- wrapCallBack cb
      let
        workers = raceAll_
          (replicate
            startupConf.workersCount
            (runWorker tq bwrapEnvs))
      workers

wrapCallBack :: (Action -> IO ()) -> IO (Either SomeException ((MessageId, ChatId), SessionResult) -> IO ())
wrapCallBack callback = do
  tid <- myThreadId
  pure \case
    Left err -> throwTo tid err
    Right ((msg, chat), sessionRes) -> callback (AnswerToUser msg chat sessionRes)

runTg
  :: AcidState IdStorage
  -> GhcPath
  -> (BS.ByteString -> Maybe GhcPath)
  -> Text
  -> TaskQueue ((MessageId, ChatId), SessionConfig) ((MessageId, ChatId), SessionResult)
  -> IO (Action -> IO ())
runTg idStorage defPath getPath token tq = do
  env <- defaultTelegramClientEnv (Token token)
  startBotAsync echoBot env
  where

    echoBot :: BotApp Model Action
    echoBot = BotApp
      { botInitialModel = ()
      , botAction = updateToAction
      , botHandler = handleAction
      , botJobs = []
      }

    updateToAction :: Update -> Model -> Maybe Action
    updateToAction update _ = case updateMessageText update of
      Just t
        | Just c <- transform <$> T.stripPrefix "/runhaskell" t
        , Just msgId <- update.updateMessage.messageMessageId
        , Just chatId <- update.updateMessage.messageChat.chatId
          -> Just (RunCommand msgId chatId c)
      _      -> Nothing

    transform t = let
      scripts = [("core", Core), ("run", Run)]
      optLevels = [("O0", O0), ("O1", O1), ("O2", O2)]

      findFrom list = find (\x -> any (==x) (map fst list ))

      mkFrom list x = snd . fromJust . find (\(y, _) -> x == y) $ list

      findWithDef def list xs = maybe def (mkFrom list) mx
        where
          mx = findFrom list xs
      bs = BS.fromStrict . encodeUtf8 $ t
      (BS.words -> header', body) = BS.break (=='\n') bs
      header   = filter (not . BS.isPrefixOf "@") header'
      script   = findWithDef Run scripts header
      optLevel = findWithDef O0 optLevels header
      ghcPath  = fromMaybe defPath (getPath =<< find (BS.isPrefixOf "ghc") header')
      in MkSessionConfig script optLevel ghcPath body

    handleAction :: Action -> Model -> Eff Action Model
    handleAction (RunCommand userMsgId chatId action) model = model <# do
      void $ makePlaceholder idStorage userMsgId chatId
      res <- liftIO $ atomically (optional $ pushTask tq ((userMsgId, chatId), action))
      case res of
        Nothing -> replyText "The server is busy now, please try later!"
        Just () -> do
          pure ()

    handleAction (AnswerToUser userMsgId chatId result) model = model <# do
      mplaceholder <- liftIO $ lookupStorage userMsgId chatId idStorage
      case mplaceholder of
        Nothing -> liftIO $ putStrLn "Can't find placeholder"
        Just botMsgId -> do
          let msgId = messageMessageId . (\(EditedMessage m) -> m) . responseResult
          msg <- fmap msgId $ liftClientM $ editMessageText EditMessageTextRequest
            {
              editMessageTextChatId = Just (SomeChatId chatId),
              editMessageTextMessageId = Just botMsgId,
              editMessageTextText = prettySessionResult result,
              editMessageTextParseMode = Nothing,
              editMessageTextInlineMessageId = Nothing,
              editMessageEntities = Nothing,
              editMessageTextDisableWebPagePreview = Nothing,
              editMessageTextReplyMarkup = Nothing
            }
          liftIO $ updateStorage userMsgId chatId msg idStorage

makePlaceholder :: AcidState IdStorage -> MessageId -> ChatId -> BotM MessageId
makePlaceholder idStorage userMsg chatId = do
  m_msg <- liftIO $ lookupStorage userMsg chatId idStorage
  case m_msg of
    Just msg -> pure msg
    Nothing -> do
      botMsg <- replyToUser userMsg chatId "Got it!"
      liftIO $ insertStorage userMsg chatId botMsg idStorage
      pure botMsg

replyToUser :: MessageId -> ChatId -> Text -> BotM MessageId
replyToUser msgId chatId content =  fmap getMsgId $ liftClientM
  (sendMessage SendMessageRequest
  { sendMessageChatId = SomeChatId chatId
  , sendMessageText = content
  , sendMessageParseMode = Nothing
  , sendMessageEntities = Nothing
  , sendMessageDisableWebPagePreview = Nothing
  , sendMessageDisableNotification = Nothing
  , sendMessageProtectContent = Nothing
  , sendMessageReplyToMessageId = Just msgId
  , sendMessageAllowSendingWithoutReply = Nothing
  , sendMessageReplyMarkup = Nothing
  }) where
  getMsgId = messageMessageId . responseResult

type Model = ()

data Action
  = RunCommand MessageId ChatId SessionConfig
  | AnswerToUser MessageId ChatId SessionResult
