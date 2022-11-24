{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Reader             (ReaderT (runReaderT))
import qualified Data.ByteString.Lazy.Char8       as BS
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Playground.Docker                (initDockerEnv)
import           Playground.Files                 (withPlaygroundRuntimeDir)
import           Playground.Session               (runPlaygroundSession)
import           Playground.Types.Docker          (DockerEnv)
import           Playground.Types.Script          (Script (Run))
import           Playground.Types.SessionConfig   (SessionConfig (MkSessionConfig))
import           Playground.Types.StartupConfig   (StartupConfig (..))
import           System.Envy                      (decodeEnv)
import           Telegram.Bot.API                 (Token (Token), Update,
                                                   defaultTelegramClientEnv)
import           Telegram.Bot.Simple              (BotApp (..), replyText,
                                                   startBot_, (<#))
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

main :: IO ()
main = do
  e <- decodeEnv
  case e of
    Left err -> putStrLn err
    Right startupConf@MkStartupConfig{tgToken} ->
      withPlaygroundRuntimeDir \runtimeDir -> do
        dockerEnv <- initDockerEnv startupConf runtimeDir
        runTg tgToken dockerEnv

runTg :: Text -> DockerEnv -> IO ()
runTg token dockerEnv = do
  env <- defaultTelegramClientEnv (Token token)
  startBot_ echoBot env
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
      Just t ->  MkSessionConfig Run . transform <$> T.stripPrefix "/runhaskell" t
      _   -> Nothing

    transform = BS.fromStrict . encodeUtf8

    handleAction action _ = () <# do
        res <- liftIO $ runReaderT (runPlaygroundSession action) dockerEnv
        replyText (decodeUtf8 $ BS.toStrict res)

type Model = ()

type Action = SessionConfig
