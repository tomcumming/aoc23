module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

parseLine :: P.ReadP (Int, ([Int], [Int]))
parseLine = do
  void $ P.string "Card"
  P.skipSpaces
  n <- readsRead
  void $ P.string ":"
  lhs <- P.many1 num
  P.skipSpaces
  void $ P.string "|"
  rhs <- P.many1 num
  pure (n, (lhs, rhs))
 where
  num = P.skipSpaces >> readsRead

matches :: [Int] -> [Int] -> [Int]
matches xs ys = filter (`S.member` S.fromList ys) xs

day4 :: [([Int], [Int])] -> Integer
day4 =
  fmap (uncurry matches)
    >>> filter (/= [])
    >>> fmap (length >>> pred >>> ((2 :: Integer) ^))
    >>> sum

part2 :: [(Int, ([Int], [Int]))] -> Integer
part2 = go mempty >>> sum
 where
  go :: M.Map Int Integer -> [(Int, ([Int], [Int]))] -> M.Map Int Integer
  go scores = \case
    [] -> scores
    (n, (xs, ys)) : rest ->
      let ixs = zipWith (const (n +)) (matches xs ys) [1 ..]
          c = M.lookup n scores & maybe 1 succ
          scores' = foldl' (\s i -> M.insertWith (+) i c s) scores ixs
       in go (M.insertWith (+) n 1 scores') rest

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure (fmap snd >>> day4)
      ["part2"] -> pure part2
      _ -> fail "Unexpected args"

  getContents'
    >>= (lines >>> pure)
    >>= traverse (readsFail parseLine)
    >>= (f >>> print)
