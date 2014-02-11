module Antemodulum.Numeric (
  toFloat,
  toDouble,
  mean,
  meanOf,
  bounded,
  boundedInt,
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.Strict (Pair(..))
import qualified Data.Foldable as Foldable

--------------------------------------------------------------------------------

-- | A specialized version of 'fromIntegral'.
toFloat :: Integral a => a -> Float
toFloat = fromIntegral

-- | A specialized version of 'fromIntegral'.
toDouble :: Integral a => a -> Double
toDouble = fromIntegral

type Int2 = Pair Int Int

-- | Mean of the elements in a container.
mean :: Foldable f => f Int -> Float
mean xs = toFloat added / toFloat len
  where
    len :!: added = Foldable.foldl' k (0 :!: 0) xs
    k :: Int2 -> Int -> Int2
    k (n :!: s) x = succ n :!: s + x

-- | Mean fraction of the elements satisfying a condition.
meanOf :: (Functor f, Foldable f) => (a -> Bool) -> f a -> Float
meanOf cond = mean . fmap (\x -> if cond x then 1 else 0)

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
