module Antemodulum.Concurrent (
  forkThrow,
  mapConcurrentlyN,
  mapConcurrentlyC,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.Monad

import Control.Concurrent.Lifted as Export
import Control.Concurrent.Async.Lifted as Export

--------------------------------------------------------------------------------

-- | Like 'fork' but always throws an exception back to the main thread.
forkThrow :: MonadBaseControl IO m => m () -> m ThreadId
forkThrow m = do
  tid <- myThreadId
  fork $ m `catchAny` (\e -> throwTo tid e >> throwIO e)

-- | 'mapConcurrentlyN' with the number of workers determined by 'getNumCapabilities'.
mapConcurrentlyC :: (MonoTraversable t, MonadIO m, MonadBaseControl IO m) => (Element t -> m b) -> t -> m [b]
mapConcurrentlyC f jobs = do
  w <- liftIO getNumCapabilities
  mapConcurrentlyN (max 1 (w - 1)) f jobs

-- | 'mapConcurrently' with a given number of workers.
mapConcurrentlyN :: (MonoTraversable t, MonadBaseControl IO m) => Int -> (Element t -> m b) -> t -> m [b]
mapConcurrentlyN n f jobs =
  concat <$> mapM (mapConcurrently f) (breakL $ toList jobs)
  where
    breakL :: [a] -> [[a]]
    breakL [] = []
    breakL xs = let (nxs, rest) = splitAt n xs in nxs : breakL rest
