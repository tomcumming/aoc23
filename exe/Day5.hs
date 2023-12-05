module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import System.Environment (getArgs)
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

type Interval = (Int, Int)

interval :: Int -> Int -> Maybe Interval
interval s e
  | s <= e = Just (s, e)
  | otherwise = Nothing

iSub :: Interval -> Interval -> [Interval]
iSub (s1, e1) (s2, e2) =
  catMaybes
    [interval (max s1 (succ e2)) e1, interval s1 (min e1 (pred s2))]

intersect :: Interval -> Interval -> Maybe Interval
intersect (s1, e1) (s2, e2) = interval (max s1 s2) (min e1 e2)

translate :: Int -> Interval -> Interval
translate n (s, e) = (s + n, e + n)

parseInput :: P.ReadP ([(Int, Int)], [[(Int, Int, Int)]])
parseInput = do
  void $ P.string "seeds:"
  seeds <- P.many1 parseSeeds
  ss <- P.many1 parseSection
  P.skipSpaces
  pure (seeds, ss)
 where
  parseSeeds = do
    P.skipSpaces
    start <- readsRead @Int
    P.skipSpaces
    end <- readsRead @Int
    pure (start, end)
  parseSection = do
    P.skipSpaces
    _name <- P.munch1 (isSpace >>> not)
    P.skipSpaces
    void $ P.string "map:"
    P.many1 sectionLine
  sectionLine = do
    P.skipSpaces
    dst <- readsRead @Int
    P.skipSpaces
    src <- readsRead @Int
    P.skipSpaces
    cnt <- readsRead @Int
    pure (dst, src, cnt)

handleInput ::
  ((Int, Int) -> [Interval]) ->
  ([(Int, Int)], [[(Int, Int, Int)]]) ->
  ([Interval], [[(Interval, Int)]])
handleInput f (seeds, swaps) =
  ( foldMap f seeds
  , fmap (\(dst, src, l) -> ((src, pred $ src + l), dst - src)) <$> swaps
  )

swapSection :: Interval -> [(Interval, Int)] -> S.Set Interval
swapSection i = \case
  [] -> S.singleton i
  (j, t) : ss ->
    foldMap (translate t >>> S.singleton) (i `intersect` j)
      <> foldMap (`swapSection` ss) (iSub i j)

swapInterval :: Interval -> [[(Interval, Int)]] -> S.Set Interval
swapInterval i = \case
  [] -> S.singleton i
  s : ss -> swapSection i s & foldMap (`swapInterval` ss)

day5 :: ([Interval], [[(Interval, Int)]]) -> Int
day5 (seeds, swaps) =
  foldMap (`swapInterval` swaps) seeds
    & toList
    & fmap fst
    & minimum

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure $ \(a, b) -> [(a, a), (b, b)]
      ["part2"] -> pure $ \(a, b) -> [(a, pred $ a + b)]
      _ -> fail "Unexpected args"
  getContents'
    >>= readsFail parseInput
    >>= (handleInput f >>> day5 >>> print)
