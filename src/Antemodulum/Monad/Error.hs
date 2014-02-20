{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antemodulum.Monad.Error (
  eitherError,
  maybeError,
  throwUnless,
  throwWhen,
  mapError,
  TErrorT,
  reportOnError,
  failOnError,
  returnError,
  prefixError,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.Arrow
import Antemodulum.ClassyPrelude
import Antemodulum.Monad

import Control.Monad.Error as Export (ErrorT(..), mapErrorT)
import Control.Monad.Error.Class as Export

--------------------------------------------------------------------------------

-- | Lift an 'Either' to a 'MonadError'.
eitherError :: MonadError e m => Either e a -> m a
eitherError = either throwError return

-- | Lift a 'Maybe' to a 'MonadError'.
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = maybe (throwError e) return

-- | 'throwError' if the condition is 'False'.
throwUnless :: MonadError e m => e -> Bool -> m ()
throwUnless err cond = unless cond $ throwError err

-- | 'throwError' if the condition is 'True'.
throwWhen :: MonadError e m => e -> Bool -> m ()
throwWhen err cond = when cond $ throwError err

--------------------------------------------------------------------------------

-- | Map the error type of an 'ErrorT'.
mapError :: Monad m => (e -> e') -> ErrorT e m a -> ErrorT e' m a
mapError f m = ErrorT $ liftM (left f) $ runErrorT m

--------------------------------------------------------------------------------

instance Error Text where
  strMsg = pack

type TErrorT = ErrorT Text

-- | Run an 'ErrorT' and use the reporting function argument on error.
reportOnError :: (MonadIO m, Exception e) => (e -> IO b) -> ErrorT e m a -> m a
reportOnError report m = runErrorT m >>= \case
  Left  err -> liftIO (report err >> throwIO err)
  Right res -> return res

-- | Run an 'ErrorT' and 'fail' on error.
failOnError :: (Monad m, Show e) => ErrorT e m a -> m a
failOnError m = runErrorT m >>= either (fail . show) return

-- | Run an 'ErrorT' and return the error.
returnError :: (MonadIO m, MonadBaseControl IO m, Exception e) => ErrorT e m a -> m e
returnError m = do
  ref <- newIORef Nothing
  catch (reportOnError (writeIORef ref . Just) m >> fail "Antemodulum.Monad.Error.returnError") $ \(_ :: SomeException) ->
    readIORef ref >>= \case
      Nothing  -> fail "Antemodulum.Monad.Error.returnError: Expected Just, got Nothing."
      Just err -> return err

-- | Insert a prefix on the error type of an 'ErrorT'.
prefixError :: (Monad m, Show e) => Text -> ErrorT e m a -> TErrorT m a
prefixError p = mapError (\s -> mconcat [p, ": ", tshow s])
