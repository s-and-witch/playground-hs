{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
import           Playground.Types.GhcVersion      (GhcVer (..))
import           Playground.Types.OptLevel        (OptLevel (..))
import           Playground.Types.Script          (Script (..))
import           Playground.Types.SessionConfig   (SessionConfig (MkSessionConfig))
import           Playground.Types.StartupConfig   (StartupConfig (..))
import           System.Envy                      (decodeEnv)
import           Telegram.Bot.API                 (Token (Token), Update,
                                                   defaultTelegramClientEnv)
import           Telegram.Bot.Simple              (BotApp (..), replyText,
                                                   startBot_, (<#))
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  e <- decodeEnv
  print e
  case e of
    Left err -> putStrLn err
    Right startupConf@MkStartupConfig{tgToken} ->
      withPlaygroundRuntimeDir \runtimeDir -> do
        dockerEnv <- initDockerEnv startupConf runtimeDir
        putStrLn "Docker env initialized!"
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
      Just t -> transform <$> T.stripPrefix "/runhaskell" t
      _      -> Nothing

    transform t = let
      scripts = [("core", Core), ("run", Run)]
      ghcVersions = [("ghc8107", GHC1), ("ghc902", GHC2), ("ghc924", GHC3), ("ghc942", GHC4)]
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
      ghcVer   = findWithDef GHC2 ghcVersions header
      optLevel = findWithDef O0 optLevels header
      in MkSessionConfig script optLevel ghcVer body


    handleAction action _ = () <# do
      res <- liftIO $ runReaderT (runPlaygroundSession action) dockerEnv
      replyText (decodeUtf8 $ BS.toStrict res)

type Model = ()

type Action = SessionConfig
