{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antemodulum.List.NonEmpty (
  singleton,
  groupBy1',
  groupJusts,
  combineOverlaps,
  snocList,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude hiding (singleton)
import Data.List.NonEmpty as Export
import Control.DeepSeq

--------------------------------------------------------------------------------

instance NFData a => NFData (NonEmpty a) where
  rnf (x :| xs) = rnf x `seq` rnf xs

singleton :: a -> NonEmpty a
singleton x = x :| []

-- | Similar to 'Data.List.NonEmpty.groupBy1' but with a function argument that
-- only needs transitivity, not equality.
--
-- See the following on how 'groupBy' expects equality:
-- http://stackoverflow.com/questions/1316365/haskell-surprising-behavior-of-groupby
groupBy1' :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1' cmp = go
  where
    go (x:|[]) = (x :| []) :| []
    go (x:|xs@(x2:rest)) =
      case grouped of
         [] ->  (x :| []) <| go (x2:|rest)
         (g:gs) -> let grouping = g :| gs in
             case ungrouped of
                 []     -> grouping :| []
                 (u:us) -> grouping <| go (u:|us)
      where
        (grouped, ungrouped) = spanNeighbor cmp (x:xs)

-- | Run a comparison function among neighbors.
spanNeighbor :: (a -> a -> Bool) -> [a] -> ([a],[a])
spanNeighbor p = go False
  where
    go _ (x1:rest@(x2:_))
       | p x1 x2   = let (ys, zs) = go True rest in (x1:ys,zs)
    go carry (x1:rest)
       | carry     = ([x1], rest)
    go _ xs      = ([], xs)

-- | Group neighboring 'Just' values together into 'NonEmpty' lists.
groupJusts :: [Maybe a] -> [NonEmpty a]
groupJusts [] = []
groupJusts xs@(_:rest) =
  case grouped of
    [] -> groupJusts rest
    (g:gs) -> (g:|gs) : groupJusts ungrouped
  where
    (grouped, ungrouped) = spanJusts xs

spanJusts :: [Maybe a] -> ([a], [Maybe a])
spanJusts [] = ([], [])
spanJusts (Nothing:rest) = ([], rest)
spanJusts (Just x :rest) = let (ys, ungrouped) = spanJusts rest
                           in  (x:ys, ungrouped)

-- | An overlapping traverse of a 'NonEmpty', two elements at a time: combine
-- the elements to a 'NonEmpty' if the condition is met, otherwise create
-- singletons.
combineOverlaps :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
combineOverlaps = combineSingleSnocIf singleton snoc
  where
    -- | perhaps can change this to not do an inefficient snoc append
    --
    -- can probably leverage reducers
    -- http://hackage.haskell.org/packages/archive/reducers/3.0.2/doc/html/Data-Semigroup-Reducer.html
    combineSingleSnocIf :: (x -> xs) -> (xs -> x -> xs) -> (x -> x -> Bool) -> NonEmpty x -> NonEmpty xs
    combineSingleSnocIf singler appender = combineIf
      where
        combineIf cond (x:|xs) = combine x (singler x) xs
          where
            combine _ combined [] = combined :| []
            combine lastB combined (b1:bs)
              | cond lastB b1 = combine b1 (appender combined b1) bs
              | otherwise     = combined <| combine b1 (singler b1) bs

-- | Inefficient 'snoc' onto a list.
snocList :: [a] -> a -> NonEmpty a
snocList xs x = Export.fromList $ xs ++ [x]
