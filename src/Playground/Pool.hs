{-# LANGUAGE BlockArguments #-}

module Playground.Pool where

import           Control.Concurrent.STM     (atomically, newTVarIO, readTVar,
                                             retry, writeTVar)
import           Data.Functor               (($>))
import           Data.Pool                  (Pool, createPool, withResource)
import           Playground.Types.Workspace (Workspace (MkWorkspace))
import           UnliftIO                   (MonadUnliftIO (withRunInIO))

makeWorkspacePool :: Int -> IO (Pool Workspace)
makeWorkspacePool n = do
  q <- newTVarIO $ map MkWorkspace [ 1 .. n ]
  let createResource = do
        r <- atomically do
          r <- readTVar q
          case r of
            []     -> retry
            (x:xs) -> writeTVar q xs $> x
        pure r
  let freeResource r = atomically $ readTVar q >>= writeTVar q . (r:)
  createPool createResource freeResource 1 (1000000 * 1000000 * 1000000 :: Double) n

withResource' :: (MonadUnliftIO m) => Pool a -> (a -> m b) -> m b
withResource' pool callback = withRunInIO \run ->
  withResource pool (\x -> run (callback x))
