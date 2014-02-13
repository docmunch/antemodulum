module Antemodulum.Strict.Pair (
  Pair(..),
  fstPair,
  sndPair,
  curryPair,
  uncurryPair
) where

--------------------------------------------------------------------------------

import Data.Strict.Tuple

--------------------------------------------------------------------------------

-- | 'fst'
fstPair :: Pair a b -> a
fstPair = fst

-- | 'snd'
sndPair :: Pair a b -> b
sndPair = snd

-- | 'curry'
curryPair :: (Pair a b -> c) -> a -> b -> c
curryPair = curry

-- | 'uncurry'
uncurryPair :: (a -> b -> c) -> Pair a b -> c
uncurryPair = uncurry
