module Main (main) where

import Control.Category ((>>>))
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (..))
import Data.Set qualified as S
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

symbolNeighbours :: M.Map Coord Entry -> S.Set Coord
symbolNeighbours es =
  es
    & M.filter isLeft
    & M.keysSet
    & foldMap neighbours
    & foldMap ((`M.lookup` numberPositions es) >>> maybe mempty S.singleton)

day3 :: M.Map Coord Entry -> Integer
day3 es =
  es
    & symbolNeighbours
    & foldMap (flip M.lookup es >>> maybeToList)
    & foldMap (either (const mempty) Sum)
    & getSum

main :: IO ()
main = getContents' >>= (parseLines >>> day3 >>> print)
