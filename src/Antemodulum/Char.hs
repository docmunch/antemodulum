module Antemodulum.Char (
  module Export
) where

--------------------------------------------------------------------------------

-- 'toLower' and 'toUpper' conflict with 'Antemodulum.ClassyPrelude'.
import Data.Char as Export hiding (toLower, toUpper)
