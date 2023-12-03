module Main (main) where

import Control.Arrow (second)
import Control.Category ((>>>))
import Data.Char (isDigit)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (Product (..), Sum (..))
import Data.Set qualified as S
import Data.Tuple (swap)
import System.Environment (getArgs)
import System.IO (getContents')

type Entry = Either Char Integer

type Coord = (Int, Int)

parseLine :: String -> M.Map Int Entry
parseLine = go 0
 where
  go x = \case
    s@(c : _)
      | isDigit c
      , ds <- takeWhile isDigit s
      , l <- length ds ->
          M.insert x (Right $ read ds) $ go (x + l) (drop l s)
    '.' : s -> go (succ x) s
    sym : s -> M.insert x (Left sym) $ go (succ x) s
    [] -> mempty

parseLines :: String -> M.Map Coord Entry
parseLines =
  lines
    >>> fmap parseLine
    >>> zipWith (\y -> M.mapKeys (,y)) [0 ..]
    >>> M.unions

-- | Position -> Original Position
numberPositions :: M.Map Coord Entry -> M.Map Coord Coord
numberPositions =
  M.mapMaybe (either (const Nothing) Just)
    >>> M.toList
    >>> foldMap (uncurry numberCoords)
 where
  numberCoords :: Coord -> Integer -> M.Map Coord Coord
  numberCoords (x, y) i =
    M.fromList
      [((x + xd, y), (x, y)) | xd <- [0 .. length (show i) - 1]]

neighbours :: Coord -> S.Set Coord
neighbours (x, y) =
  S.fromList $ [(x + xd, y + yd) | xd <- [-1 .. 1], yd <- [-1 .. 1]]

numberNeighbours :: M.Map Coord Entry -> Coord -> S.Set Coord
numberNeighbours es = neighbours >>> foldMap findNum
 where
  findNum = (`M.lookup` numberPositions es) >>> maybe mempty S.singleton

symbolNeighbours :: M.Map Coord Entry -> M.Map Char (S.Set (S.Set Coord))
symbolNeighbours es =
  es
    & M.mapMaybe (either Just (const Nothing))
    & M.toList
    & fmap (swap >>> second (numberNeighbours es >>> S.singleton))
    & M.fromListWith (<>)

day3 :: M.Map Coord Entry -> Integer
day3 es =
  es
    & symbolNeighbours
    & foldMap fold
    & foldMap (flip M.lookup es >>> maybeToList)
    & foldMap (either (const mempty) Sum)
    & getSum

day3part2 :: M.Map Coord Entry -> Integer
day3part2 es =
  es
    & symbolNeighbours
    & M.lookup '*'
    & fromMaybe mempty
    & S.filter (S.size >>> (== 2))
    & foldMap
      ( foldMap
          ( flip M.lookup es
              >>> maybeToList
              >>> foldMap (either (const mempty) Product)
          )
          >>> getProduct
          >>> Sum
      )
    & getSum

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure day3
      ["part2"] -> pure day3part2
      _ -> fail "Unexpected args"
  getContents' >>= (parseLines >>> f >>> print)
