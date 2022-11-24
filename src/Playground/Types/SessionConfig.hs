module Playground.Types.SessionConfig where
  
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Playground.Types.Script    (Script)


data SessionConfig = MkSessionConfig
  { script  :: Script
  , content :: ByteString
  }
