{-# LANGUAGE BlockArguments #-}

module Playground.TaskQueue where

import           Control.Concurrent.STM
import           Control.Exception          (throwIO, try)
import           GHC.Natural                (Natural)
import           Playground.Types.TaskQueue (TaskCallback (..), TaskQueue (..))

pushTask :: TaskQueue arg res -> arg -> STM (IO res)
pushTask (MkTaskQueue queue) val = do
  callback <- newEmptyTMVar
  writeTBQueue queue (val, MkTaskCallback callback)
  pure $ (atomically (takeTMVar callback)) >>= either throwIO pure

newTaskQueue :: Natural -> IO (TaskQueue argument result)
newTaskQueue = (fmap . fmap) MkTaskQueue newTBQueueIO

readTaskQueue :: TaskQueue arg res -> IO (arg, TaskCallback res)
readTaskQueue (MkTaskQueue queue) = atomically $ readTBQueue queue

returnTaskResult :: TaskCallback res -> IO res -> IO ()
returnTaskResult (MkTaskCallback callback) action =
  try action >>= atomically . putTMVar callback
