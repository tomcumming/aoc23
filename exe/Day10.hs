module Main (main) where

import AOC.IO (readsFail)
import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

type Row = Int

type Col = Int

data Tile
  = Vert
  | Horiz
  | NE
  | NW
  | SW
  | SE
  | Start
  | Ground
  deriving (Eq, Show)

parseInput :: P.ReadP [[Tile]]
parseInput = P.many1 parseLine
 where
  parseLine = P.manyTill parseTile (P.string "\n")
  parseTile =
    P.get >>= \case
      '|' -> pure Vert
      '-' -> pure Horiz
      'L' -> pure NE
      'J' -> pure NW
      '7' -> pure SW
      'F' -> pure SE
      'S' -> pure Start
      '.' -> pure Ground
      _ -> fail "Unknown tile"

intoMap :: [[Tile]] -> M.Map (Col, Row) Tile
intoMap =
  fmap (zip [0 ..])
    >>> zipWith (\r -> fmap (\(c, t) -> ((c, r), t))) [0 ..]
    >>> concat
    >>> M.fromList

neighbours :: M.Map (Col, Row) Tile -> (Col, Row) -> S.Set (Col, Row)
neighbours m (c, r) =
  M.lookup (c, r) m & \case
    Just Vert -> S.fromList [(c, r + 1), (c, r - 1)]
    Just Horiz -> S.fromList [(c + 1, r), (c - 1, r)]
    Just NE -> S.fromList [(c, r - 1), (c + 1, r)]
    Just NW -> S.fromList [(c, r - 1), (c - 1, r)]
    Just SW -> S.fromList [(c, r + 1), (c - 1, r)]
    Just SE -> S.fromList [(c, r + 1), (c + 1, r)]
    _ -> mempty

starts :: M.Map (Col, Row) Tile -> S.Set (Col, Row)
starts = M.filter (== Start) >>> M.keysSet

startNeighbours :: M.Map (Col, Row) Tile -> S.Set (Col, Row)
startNeighbours m =
  M.mapWithKey (\k _ -> neighbours m k) m
    & M.filter (S.intersection (starts m) >>> S.null >>> not)
    & M.keysSet

steps :: M.Map (Col, Row) Tile -> [S.Set (Col, Row)]
steps m = starts m : startNeighbours m : go (startNeighbours m)
 where
  go v =
    let v' = foldMap (neighbours m) v
        ns = v' `S.difference` v
     in if S.null ns then [] else ns : go (v `S.union` v')

main :: IO ()
main =
  getContents'
    >>= readsFail parseInput
    >>= (intoMap >>> steps >>> length >>> pred >>> print)
