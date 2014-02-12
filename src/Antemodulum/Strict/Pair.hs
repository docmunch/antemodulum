-- | same functionality as Data.Strict.Pair
-- However, add a Pair suffix on functions to avoid prelude conflicts
module Antemodulum.Strict.Pair (
  Pair(..), fstPair, sndPair, curryPair, uncurryPair
  ) where

import Data.Strict.Tuple

fstPair = fst
sndPair = snd
curryPair = curry
uncurryPair = uncurry
