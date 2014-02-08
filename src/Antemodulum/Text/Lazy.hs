module Antemodulum.Text.Lazy (
  TextL,
  module Export
) where

--------------------------------------------------------------------------------

import Data.Text.Lazy as Export
import Data.Text.Lazy.IO as Export

--------------------------------------------------------------------------------

-- | Lazy 'Text' type synonmym
type TextL = Text
