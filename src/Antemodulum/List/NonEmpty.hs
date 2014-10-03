{-# OPTIONS_GHC -fno-warn-orphans #-}

module Antemodulum.List.NonEmpty (
  groupBy1Transitive,
  snocList,
  module Export
) where

--------------------------------------------------------------------------------

import Antemodulum.ClassyPrelude
import Antemodulum.DeepSeq
import Data.List.NonEmpty as Export

--------------------------------------------------------------------------------

-- | _O(n)_ 'snoc' onto a list.
snocList :: [a] -> a -> NonEmpty a
snocList xs x = Export.fromList $ xs ++ [x]

-- | Similar to 'Data.List.NonEmpty.groupBy1' but with a function argument that
-- only needs transitivity, not equality.
--
-- See the following on how 'groupBy' expects equality:
-- http://stackoverflow.com/questions/1316365/haskell-surprising-behavior-of-groupby
groupBy1Transitive :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1Transitive cmp = go
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
