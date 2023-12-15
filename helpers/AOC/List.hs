module AOC.List (split) where

import Control.Category ((>>>))
import Data.Foldable (toList)

split :: forall a t. (Eq a, Foldable t) => a -> t a -> [[a]]
split sep = toList >>> go []
 where
  go existing = \case
    x : xs | x == sep -> pushNonNull existing (go [] xs)
    x : xs -> go (x : existing) xs
    [] -> pushNonNull existing []
  pushNonNull x xs
    | null x = xs
    | otherwise = reverse x : xs
