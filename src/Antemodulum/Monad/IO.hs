module Antemodulum.Monad.IO (
  liftIO1,
  liftIO2,
  liftIO3,
  liftIO4,
  liftIO5,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Control.Monad.IO.Class as Export

--------------------------------------------------------------------------------

liftIO1 :: MonadIO m => (a1 -> IO a) -> a1 -> m a
liftIO1 f a1 = liftIO (f a1)

liftIO2 :: MonadIO m => (a1 -> a2 -> IO a) -> a1 -> a2 -> m a
liftIO2 f a1 a2 = liftIO (f a1 a2)

liftIO3 :: MonadIO m => (a1 -> a2 -> a3 -> IO a) -> a1 -> a2 -> a3 -> m a
liftIO3 f a1 a2 a3 = liftIO (f a1 a2 a3)

liftIO4 :: MonadIO m => (a1 -> a2 -> a3 -> a4 -> IO a) -> a1 -> a2 -> a3 -> a4 -> m a
liftIO4 f a1 a2 a3 a4 = liftIO (f a1 a2 a3 a4)

liftIO5 :: MonadIO m => (a1 -> a2 -> a3 -> a4 -> a5 -> IO a) -> a1 -> a2 -> a3 -> a4 -> a5 -> m a
liftIO5 f a1 a2 a3 a4 a5 = liftIO (f a1 a2 a3 a4 a5)
