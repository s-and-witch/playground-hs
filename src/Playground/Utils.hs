module Playground.Utils where
  
import           Control.Concurrent.Async (race_)

raceAll_ :: [IO ()] -> IO ()
raceAll_ []     = pure ()
raceAll_ [x]    = x
raceAll_ (x:xs) = race_ x (raceAll_ xs)
