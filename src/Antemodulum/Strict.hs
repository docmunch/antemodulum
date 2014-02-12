module Antemodulum.Strict (
  EitherS,
  MaybeS,
  module Export
) where

--------------------------------------------------------------------------------

import Data.Strict as Export

--------------------------------------------------------------------------------

-- | Strict 'Either' type synonmym
type EitherS = Either

-- | Strict 'Maybe' type synonmym
type MaybeS = Maybe
