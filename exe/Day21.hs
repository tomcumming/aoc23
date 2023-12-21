module Main (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import System.IO (getContents')

data Tile = Start | Wall | Floor deriving (Eq, Show)

parseInput :: (MonadFail m) => String -> m [[Tile]]
parseInput = lines >>> traverse (traverse parseTile)
 where
  parseTile = \case
    'S' -> pure Start
    '.' -> pure Floor
    '#' -> pure Wall
    c -> "Cant parse: " <> [c] & fail

type Coord = (Int, Int)

makeGrid :: [[Tile]] -> M.Map Coord Tile
makeGrid =
  zipWith
    (\y -> zipWith (\x t -> ((x, y), t)) [0 ..])
    [0 ..]
    >>> concat
    >>> M.fromList

neighbours :: M.Map Coord Tile -> Coord -> S.Set Coord
neighbours grid (x, y) =
  ( do
      c <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      M.lookup c grid & \case
        Just Floor -> pure c
        Just Start -> pure c
        _ -> []
  )
    & S.fromList

startCoords :: M.Map Coord Tile -> S.Set Coord
startCoords = M.filter (== Start) >>> M.keysSet

step :: M.Map Coord Tile -> S.Set Coord -> S.Set Coord
step grid = foldMap (neighbours grid)

part1 :: M.Map Coord Tile -> Int -> S.Set Coord
part1 grid = go (startCoords grid)
 where
  go :: S.Set Coord -> Int -> S.Set Coord
  go cs = \case
    0 -> cs
    n -> go (step grid cs) (pred n)

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= (makeGrid >>> (`part1` 64) >>> S.size >>> print)
