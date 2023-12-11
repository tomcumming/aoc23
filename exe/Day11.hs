module Main (main) where

import AOC.IO ()
import Control.Arrow (second)
import Control.Category ((>>>))
import Data.List (transpose)
import System.IO (getContents')

parseInput :: String -> [String]
parseInput = lines

extraRows :: [[Char]] -> [[Char]]
extraRows = expand >>> transpose >>> expand >>> transpose
 where
  expand = \case
    [] -> []
    x : xs | all (== '.') x -> x : x : expand xs
    x : xs -> x : expand xs

starCoords :: [[Char]] -> [(Int, Int)]
starCoords =
  fmap (`zip` [0 :: Int ..])
    >>> zipWith (\y -> fmap (second (,y))) [0 :: Int ..]
    >>> concat
    >>> concatMap (\(c, p) -> [p | c == '#'])

distances :: [(Int, Int)] -> [Int]
distances = \case
  [] -> []
  (x : xs) -> fmap (go x) xs <> distances xs
 where
  go (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main =
  getContents'
    >>= ( parseInput
            >>> extraRows
            >>> starCoords
            >>> distances
            >>> sum
            >>> print
        )
