{-# LANGUAGE BlockArguments #-}
module Playground.Worker where

import           Control.Monad                  (forever)
import           Control.Monad.Reader           (ReaderT (runReaderT))
import           Playground.Session             (runPlaygroundSession)
import           Playground.TaskQueue           (readTaskQueue,
                                                 returnTaskResult)
import           Playground.Types.Docker        (DockerEnv)
import           Playground.Types.SessionConfig (SessionConfig)
import           Playground.Types.SessionResult (SessionResult)
import           Playground.Types.TaskQueue     (TaskQueue)


runWorker :: TaskQueue SessionConfig SessionResult -> DockerEnv -> IO ()
runWorker queue dockerEnv = forever do
  (sc, callback) <- readTaskQueue queue
  returnTaskResult callback (runSession sc)
  where
    runSession sc = runReaderT (runPlaygroundSession sc) dockerEnv
