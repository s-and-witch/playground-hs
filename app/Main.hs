{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Applicative              (optional, Alternative ((<|>)))
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
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Control.Monad (void)
import qualified Data.Text.Encoding as T

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
      cb <- runTg
        idStorage
        startupConf.ghcDefault
        (Map.keys startupConf.ghcMap)
        (startupConf.ghcMap Map.!?)
        tgToken
        tq
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
  -> [BS.ByteString]
  -> (BS.ByteString -> Maybe GhcPath)
  -> Text
  -> TaskQueue ((MessageId, ChatId), SessionConfig) ((MessageId, ChatId), SessionResult)
  -> IO (Action -> IO ())
runTg idStorage defPath ghcNames getPath token tq = do
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
        , Just msg <- update.updateMessage <|> update.updateEditedMessage
          -> Just (RunCommand msg.messageMessageId msg.messageChat.chatId c)
      Just t
        | T.isPrefixOf "/help_haskell" t
        , Just msg <- update.updateMessage <|> update.updateEditedMessage
          -> Just (HelpCommand msg.messageMessageId msg.messageChat.chatId)
      _   -> Nothing

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
      botMsg <- makePlaceholder idStorage userMsgId chatId
      res <- liftIO $ atomically (optional $ pushTask tq ((botMsg, chatId), action))
      case res of
        Nothing -> replyText "The server is busy now, please try later!"
        Just () -> do
          pure ()

    handleAction (AnswerToUser botMsgId chatId result) model = model <# do
      editMsg botMsgId chatId (prettySessionResult result)


    handleAction (HelpCommand userId chatId) model = model <# do
      m_msg <- liftIO $ lookupStorage userId chatId idStorage
      case m_msg of
        Nothing -> void $ registerNewReply idStorage userId chatId helpText
        Just msg -> editMsg msg chatId helpText
      where
        helpText = T.unlines
          [ "Runhaskell Bot - interactive haskell playground in Telegram"
          , ""
          , "/runhaskell <options>"
          , "<code>"
          , "Command to compile and run the code from message"
          , "Available options:"
          , "- `run` or `core` - run the code or generate it's core output"
          , "- `O0`, `O1`, `O2` - optimisation levels (default - O0)"
          , "- " <> ghcs
          , ""
          , "/help_haskell - show this message"
          , "Tips:"
          , "- It's highly recommended to edit your message instead of typing the new one"
          , "- There are a lot of libraries available like `massiv`, `lens` or `conduit`"
          , ""
          , "source code: https://github.com/s-and-witch/playground-hs/"
          ]

        ghcs = T.intercalate ", " (map (wrapQuotes . T.decodeUtf8 . BS.toStrict) ghcNames)
        wrapQuotes t = "`" <> t <> "`"

makePlaceholder :: AcidState IdStorage -> MessageId -> ChatId -> BotM MessageId
makePlaceholder idStorage userMsg chatId = do
  m_msg <- liftIO $ lookupStorage userMsg chatId idStorage
  case m_msg of
    Just msg -> pure msg
    Nothing -> registerNewReply idStorage userMsg chatId "Got it!"

registerNewReply :: AcidState IdStorage -> MessageId -> ChatId -> Text -> BotM MessageId
registerNewReply idStorage userMsg chatId content = do
  botMsg <- replyToUser userMsg chatId content
  liftIO $ insertStorage userMsg chatId botMsg idStorage
  pure botMsg

replyToUser :: MessageId -> ChatId -> Text -> BotM MessageId
replyToUser msgId chatId content =  (messageMessageId . responseResult ) <$> liftClientM
  (sendMessage
    (defSendMessage (SomeChatId chatId) (wrapMonospace content))
      { sendMessageParseMode = Just MarkdownV2
      , sendMessageReplyToMessageId = Just msgId
      })

editMsg :: MessageId -> ChatId -> Text -> BotM ()
editMsg msgId chatId content =
      editMessage
        (EditChatMessageId (SomeChatId chatId) msgId)
        (toEditMessage $ wrapMonospace content)
          { editMessageParseMode = Just MarkdownV2
          }

wrapMonospace :: Text -> Text
wrapMonospace content = "```\n" <> content <> "\n```"

type Model = ()

data Action
  = RunCommand MessageId ChatId SessionConfig
  | AnswerToUser MessageId ChatId SessionResult
  | HelpCommand  MessageId ChatId
