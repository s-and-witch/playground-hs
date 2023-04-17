{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Playground.TaskQueue where

import Control.Concurrent.STM     (STM, atomically, newTBQueueIO, readTBQueue,
                                   writeTBQueue)
import Control.Exception          (SomeException, try)

import GHC.Natural                (Natural)

import Playground.Types.TaskQueue (TaskQueue (..))

pushTask :: TaskQueue arg res -> arg -> STM ()
pushTask (MkTaskQueue queue _) val = do
  writeTBQueue queue (val)

newTaskQueue :: Natural -> (Either SomeException result -> IO ()) -> IO (TaskQueue argument result)
newTaskQueue count callBack = do
  queue <- newTBQueueIO count
  pure MkTaskQueue{..}

readTaskQueue :: TaskQueue arg res -> IO arg
readTaskQueue (MkTaskQueue{queue}) = atomically $ readTBQueue queue

returnTaskResult :: TaskQueue arg res -> IO res -> IO ()
returnTaskResult (MkTaskQueue {callBack}) action =
  try action >>=  callBack
