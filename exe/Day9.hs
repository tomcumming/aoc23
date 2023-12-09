module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Data.Foldable (foldl')
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

parseInput :: P.ReadP [[Int]]
parseInput = P.many parseLine
 where
  parseLine :: P.ReadP [Int]
  parseLine = P.manyTill (P.skipSpaces >> readsRead @Int) (P.string "\n")

addNumber :: Int -> [Int] -> [Int]
addNumber n = \case
  [] -> [n]
  (x : xs) -> n : addNumber (n - x) xs

initialize :: [Int] -> [Int]
initialize = foldl' (flip addNumber) []

newRow :: [Int] -> [Int]
newRow = scanr (+) 0

main :: IO ()
main =
  getContents'
    >>= readsFail parseInput
    >>= (fmap (initialize >>> newRow >>> take 1) >>> concat >>> sum >>> print)
