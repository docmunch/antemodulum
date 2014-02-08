module Antemodulum.Concurrent (
  mapConcurrentlyN,
  mapConcurrentlyC,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.Monad

import Control.Concurrent as Export (ThreadId, myThreadId, forkIO, forkFinally, forkIOWithUnmask, killThread, throwTo, forkOn, forkOnWithUnmask, getNumCapabilities, setNumCapabilities, threadCapability, threadDelay)
import Control.Concurrent.Async.Lifted as Export

--------------------------------------------------------------------------------

-- | 'mapConcurrentlyN' with the number of workers determined by 'getNumCapabilities'.
mapConcurrentlyC :: (MonadIO m, MonadBaseControl IO m) => (a -> m b) -> [a] -> m [b]
mapConcurrentlyC f jobs = do
  w <- liftIO getNumCapabilities
  mapConcurrentlyN (max 1 (w - 1)) f jobs

-- | 'mapConcurrently' with a given number of workers.
mapConcurrentlyN :: MonadBaseControl IO m => Int -> (a -> m b) -> [a] -> m [b]
mapConcurrentlyN n f jobs =
  concat <$> mapM (mapConcurrently f) (breakL jobs)
  where
    breakL :: [a] -> [[a]]
    breakL [] = []
    breakL xs = let (nxs, rest) = splitAt n xs in nxs : breakL rest
