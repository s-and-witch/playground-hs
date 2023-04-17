module Playground.Types.TaskQueue where

import Control.Concurrent.STM (TBQueue)
import Control.Exception      (SomeException)

data TaskQueue argument result = MkTaskQueue
   { queue    :: TBQueue argument
   , callBack :: Either SomeException result -> IO ()
   }
