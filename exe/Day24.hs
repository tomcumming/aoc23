module Main (main) where

import AOC.IO (readFail)
import AOC.List (split)
import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Maybe (maybeToList)
import Data.Ratio ((%))
import System.IO (getContents')

parseInput :: (MonadFail m) => String -> m [([Integer], [Integer])]
parseInput = lines >>> traverse parseLine

parseLine :: (MonadFail m) => String -> m ([Integer], [Integer])
parseLine =
  split '@'
    >>> fmap (split ',')
    >>> traverse (traverse (filter (isSpace >>> not) >>> readFail))
    >>> \case
      [[pos, vel]] -> pure (pos, vel)
      l -> "Can't parse line: " <> show l & fail

cross ::
  ([Rational], [Rational]) ->
  ([Rational], [Rational]) ->
  Maybe (Rational, Rational)
cross lhs rhs = case (lhs, rhs) of
  ((px1 : py1 : _, vx1 : vy1 : _), (px2 : py2 : _, vx2 : vy2 : _)) ->
    go px1 py1 vx1 vy1 px2 py2 vx2 vy2
  _ -> Nothing
 where
  go px1 py1 vx1 vy1 px2 py2 vx2 vy2
    | vx1 /= 0
    , vx2 /= 0
    , (a1 - a2) /= 0
    , t1 >= 0
    , t2 >= 0 =
        Just (x, a1 * x + b1)
    | otherwise = Nothing
   where
    a1 = vy1 / vx1
    b1 = negate (px1 * a1 - py1)
    a2 = vy2 / vx2
    b2 = negate (px2 * a2 - py2)
    x = negate (b1 - b2) / (a1 - a2)
    t1 = (x - px1) / vx1
    t2 = (x - px2) / vx2

asRationals :: [([Integer], [Integer])] -> [([Rational], [Rational])]
asRationals = fmap (bimap (fmap (% 1)) (fmap (% 1)))

inRange :: (Ord a) => a -> a -> (a, a) -> Bool
inRange mi ma (x, y) = x >= mi && x <= ma && y >= mi && y <= ma

part1 ::
  Rational ->
  Rational ->
  [([Rational], [Rational])] ->
  Int
part1 mi ma = \case
  [] -> 0
  p1 : rest -> go p1 rest + part1 mi ma rest
 where
  go p1 = \case
    [] -> 0
    p2 : rest ->
      let collides =
            cross p1 p2
              & maybeToList
              & filter (inRange mi ma)
              & length
       in collides + go p1 rest

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= ( asRationals
            >>> part1 200000000000000 400000000000000
            >>> print
        )
