{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antemodulum.Monad.Error (
  eitherError,
  maybeError,
  throwUnless,
  throwWhen,
  mapError,
  prefixError,
  failOnError,
  TErrorT
) where

--------------------------------------------------------------------------------

import Antemodulum.Arrow
import Antemodulum.ClassyPrelude
import Antemodulum.Monad
import Antemodulum.Text.Strict (TextS)

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

-- | Insert a prefix on the error type of an 'ErrorT'.
prefixError :: (Monad m, IsString e, Monoid e) => e -> ErrorT e m a -> ErrorT e m a
prefixError p = mapError (\s -> mconcat [p, ": ", s])

-- | Run an 'ErrorT' and 'fail' on error.
failOnError :: (Monad m, Show e) => ErrorT e m a -> m a
failOnError m = runErrorT m >>= either (fail . show) return

--------------------------------------------------------------------------------

instance Error TextS where
  strMsg = pack

type TErrorT m = ErrorT TextS m
