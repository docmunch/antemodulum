module Antemodulum.ByteString.Lazy (
  ByteStringL,
  module Export
) where

--------------------------------------------------------------------------------

import Data.ByteString.Lazy as Export

--------------------------------------------------------------------------------

-- | Lazy 'ByteString' type synonmym
type ByteStringL = ByteString
