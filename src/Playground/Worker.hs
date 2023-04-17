{-# LANGUAGE BlockArguments #-}
module Playground.Worker where

import Control.Monad                  (forever)
import Control.Monad.Reader           (ReaderT (runReaderT))

import Playground.Session             (runPlaygroundSession)
import Playground.TaskQueue           (readTaskQueue, returnTaskResult)
import Playground.Types.Bwrap         (BwrapEnv)
import Playground.Types.SessionConfig (SessionConfig)
import Playground.Types.SessionResult (SessionResult)
import Playground.Types.TaskQueue     (TaskQueue)


runWorker :: TaskQueue (a, SessionConfig) (a, SessionResult) -> BwrapEnv -> IO ()
runWorker queue bwrapEnv = forever do
  (additinalInfo, sc) <- readTaskQueue queue
  returnTaskResult queue ((additinalInfo,) <$> runSession sc)
  where
    runSession sc = runReaderT (runPlaygroundSession sc) bwrapEnv
