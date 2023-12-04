module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Set qualified as S
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

parseLine :: P.ReadP ([Int], [Int])
parseLine = do
  void $ P.string "Card"
  P.skipSpaces
  void $ P.munch1 isDigit
  void $ P.string ":"
  lhs <- P.many1 num
  P.skipSpaces
  void $ P.string "|"
  rhs <- P.many1 num
  pure (lhs, rhs)
 where
  num = P.skipSpaces >> readsRead

matches :: [Int] -> [Int] -> [Int]
matches xs ys = filter (`S.member` S.fromList ys) xs

main :: IO ()
main =
  getContents'
    >>= (lines >>> pure)
    >>= traverse (readsFail parseLine)
    >>= ( fmap (uncurry matches)
            >>> filter (/= [])
            >>> fmap (length >>> pred >>> ((2 :: Integer) ^))
            >>> sum
            >>> print
        )
