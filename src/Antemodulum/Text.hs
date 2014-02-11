module Antemodulum.Text (
 Strippable(..)
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import qualified Antemodulum.Text.Strict as TS
import qualified Antemodulum.Text.Lazy as TL

--------------------------------------------------------------------------------

-- | A class for functions that strip whitespace.
class Strippable a where
  strip :: a -> a
  stripStart :: a -> a
  stripEnd :: a -> a

instance Strippable Text where
  strip = TS.strip
  stripStart = TS.stripStart
  stripEnd = TS.stripEnd

instance Strippable TL.Text where
  strip = TL.strip
  stripStart = TL.stripStart
  stripEnd = TL.stripEnd
