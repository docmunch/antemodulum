module Antemodulum.Numeric (
  bounded,
  boundedInt,
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude

--------------------------------------------------------------------------------

-- | Convert an 'a' to a 'b' if the resulting value is within the bounds of the
-- 'b' range (assuming the range of 'a' is larger).
--
-- If 'x < minBound', the result is 'Left False'.
-- If 'x > maxBound', the result is 'Left True'.
-- Otherwise, the result is 'Right y', where 'y' is the converted value.
bounded :: (Bounded b, Ord a) => (b -> a) -> (a -> b) -> a -> Either Bool b
bounded from to x
  | x < from minBound = Left False
  | x > from maxBound = Left True
  | otherwise         = Right (to x)

-- | Convert an 'Integral' to an 'Int' if the resulting value is within the
-- 'Int' bounds.
boundedInt :: Integral a => a -> Either Bool Int
boundedInt = bounded toInteger fromInteger . toInteger
