module Playground.Types.TaskQueue where
  
import           Control.Concurrent.STM (TBQueue, TMVar)
import           Control.Exception      (SomeException)

newtype TaskQueue argument result = MkTaskQueue
  (TBQueue (argument, TaskCallback result))

newtype TaskCallback result =
  MkTaskCallback (TMVar (Either SomeException result))
