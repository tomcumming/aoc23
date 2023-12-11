module Main (main) where

import AOC.IO ()
import Control.Category ((>>>))
import Data.List (transpose)
import System.Environment (getArgs)
import System.IO (getContents')

parseInput :: String -> [String]
parseInput = lines

yCoords :: Int -> Int -> [[Char]] -> [[(Char, Int)]]
yCoords gap n = \case
  [] -> []
  l : ls | all (== '.') l -> yCoords gap (n + gap) ls
  l : ls -> fmap (,n) l : yCoords gap (succ n) ls

xCoords :: Int -> Int -> [[(Char, Int)]] -> [[(Int, Int)]]
xCoords gap n = \case
  [] -> []
  l : ls | all (fst >>> (== '.')) l -> xCoords gap (n + gap) ls
  l : ls ->
    concatMap (\(c, y) -> [(n, y) | c == '#']) l
      : xCoords gap (succ n) ls

coords :: Int -> [[Char]] -> [(Int, Int)]
coords gap = yCoords gap 0 >>> transpose >>> xCoords gap 0 >>> concat

distances :: [(Int, Int)] -> [Int]
distances = \case
  [] -> []
  (x : xs) -> fmap (go x) xs <> distances xs
 where
  go (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  gap <-
    getArgs >>= \case
      [] -> pure 2
      ["part2"] -> pure 1000000
      _ -> fail "Unexpected args"
  getContents'
    >>= ( parseInput
            >>> coords gap
            >>> distances
            >>> sum
            >>> print
        )
