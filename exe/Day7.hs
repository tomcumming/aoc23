module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Arrow (first)
import Control.Category ((>>>))
import Control.Monad (replicateM)
import Data.Char (digitToInt, isDigit)
import Data.Function ((&))
import Data.List qualified as L
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

data Rank
  = Num Int
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Show)

type Hand = ([Int], [Rank])

intoHand :: [Rank] -> Hand
intoHand rs =
  ( rs & L.sort & L.group & fmap length & L.sort & reverse
  , rs
  )

parseRank :: P.ReadP Rank
parseRank =
  P.get >>= \case
    c | isDigit c -> digitToInt c & Num & pure
    'T' -> pure Ten
    'J' -> pure Jack
    'Q' -> pure Queen
    'K' -> pure King
    'A' -> pure Ace
    _ -> fail "Unexpected Rank"

parseInput :: P.ReadP [([Rank], Int)]
parseInput = do
  rows <-
    P.many1
      ( do
          P.skipSpaces
          rs <- replicateM 5 parseRank
          P.skipSpaces
          cnd <- readsRead @Int
          pure (rs, cnd)
      )
  P.skipSpaces
  pure rows

main :: IO ()
main =
  getContents'
    >>= readsFail parseInput
    >>= ( fmap (first intoHand)
            >>> L.sort
            >>> zipWith (\x (_, y) -> x * y) [1 ..]
            >>> sum
            >>> print
        )
