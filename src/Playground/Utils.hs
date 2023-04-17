{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Playground.Utils where

import Control.Concurrent.Async (race_)
import GHC.Records

raceAll_ :: [IO ()] -> IO ()
raceAll_ []     = pure ()
raceAll_ [x]    = x
raceAll_ (x:xs) = race_ x (raceAll_ xs)


instance (HasField t a b) => HasField t (Maybe a) (Maybe b) where
  getField = fmap (getField @t)
