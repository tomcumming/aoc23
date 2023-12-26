module Main (main) where

import AOC.List (split)
import Algorithm.Search (dijkstra)
import Control.Category ((>>>))
import Data.Foldable (fold, maximumBy)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Tuple (swap)
import System.IO (getContents')
import System.Random (randomRIO)

parseInput :: String -> [(String, [String])]
parseInput = lines >>> fmap (split ':') >>> concatMap goLine
 where
  goLine = \case
    [src, dsts] -> [(src, split ' ' (drop 1 dsts))]
    _ -> []

verts :: [(String, [String])] -> S.Set (String, String)
verts = \case
  [] -> mempty
  (src, dsts) : rest -> foldMap (go src) dsts <> verts rest
 where
  go src dst = S.singleton (min src dst, max src dst)

type Neighbours = M.Map String (S.Set String)

neighbours :: [(String, String)] -> Neighbours
neighbours =
  concatMap (\p -> [p, swap p])
    >>> fmap (fmap S.singleton)
    >>> M.fromListWith (<>)

pathVerts :: [String] -> [(String, String)]
pathVerts = \case
  x : y : zs -> (min x y, max x y) : pathVerts (y : zs)
  _ -> mempty

weigh ::
  Neighbours ->
  Int ->
  M.Map (String, String) Int ->
  IO (M.Map (String, String) Int)
weigh ns n weights
  | n == 0 = pure weights
  | otherwise = do
      (a, _) <- randomRIO (0, M.size ns - 1) <&> (`M.elemAt` ns)
      (b, _) <- randomRIO (0, M.size ns - 1) <&> (`M.elemAt` ns)
      let path =
            dijkstra
              ((`M.lookup` ns) >>> fold)
              (\_ _ -> 1 :: Int)
              (== b)
              a
      let weights' = foldMap (snd >>> pathVerts) path <&> (,1) & M.fromList
      weigh ns (pred n) (M.unionWith (+) weights weights')

threeCuts ::
  [(String, String)] ->
  S.Set (String, String) ->
  IO (S.Set (String, String))
threeCuts cuts vs
  | length cuts == 3 = pure vs
  | otherwise = do
      let ns = neighbours (S.toList vs)
      weights <- weigh ns 1000 mempty
      let (cut, _) = M.toList weights & maximumBy (comparing snd)
      threeCuts (cut : cuts) (S.delete cut vs)

fill :: Neighbours -> S.Set String -> S.Set String
fill ns vs
  | vs' == vs = vs
  | otherwise = fill ns vs'
 where
  vs' = vs <> foldMap ((`M.lookup` ns) >>> fold) vs

connected :: M.Map String (S.Set String) -> S.Set (S.Set String)
connected ns =
  foldMap
    (fill ns >>> S.singleton)
    (foldMap (S.singleton >>> S.singleton) (M.keysSet ns))

main :: IO ()
main =
  getContents'
    >>= (parseInput >>> verts >>> threeCuts mempty)
    >>= (S.toList >>> neighbours >>> connected >>> mapM_ (S.size >>> print))
