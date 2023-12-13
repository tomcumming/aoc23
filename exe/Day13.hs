module Main (main) where

import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import System.Environment (getArgs)
import System.IO (getContents')

parseInput :: String -> [[String]]
parseInput = lines >>> go [] >>> fmap reverse
 where
  go ss = \case
    [] -> [ss]
    "" : ls -> ss : go [] ls
    l : ls -> go (l : ss) ls

reflection :: Int -> [String] -> [Int]
reflection ds = go []
 where
  go ss = \case
    [] -> []
    l : ls
      | not (null ls)
      , zipWith lineDiff (l : ss) ls
          & sum
          & (== ds) ->
          1 + length ss : go (l : ss) ls
    l : ls -> go (l : ss) ls

lineDiff :: String -> String -> Int
lineDiff xs = zipWith (\x y -> x == y & bool 1 0) xs >>> sum

main :: IO ()
main = do
  ds <-
    getArgs >>= \case
      [] -> pure 0
      ["part2"] -> pure 1
      _ -> fail "Unexpected args"
  getContents'
    >>= ( parseInput
            >>> concatMap
              ( \ls ->
                  (reflection ds ls <&> (* 100))
                    <> reflection ds (L.transpose ls)
              )
            >>> sum
            >>> print
        )
