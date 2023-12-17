module Main (main) where

import AOC.IO (readFail)
import Algorithm.Search (dijkstraAssoc)
import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import System.IO (getContents')

parseInput :: (MonadFail m) => String -> m [[Int]]
parseInput = lines >>> traverse (traverse (pure >>> readFail))

type Coord = (Int, Int)

intoCoords :: [[Int]] -> M.Map Coord Int
intoCoords =
  zipWith (\y -> zipWith (\x l -> ((x, y), l)) [0 ..]) [0 ..]
    >>> concat
    >>> M.fromList

data Dir = Up | Down | Lef | Righ deriving (Eq, Ord, Show)

unitDir :: Dir -> Coord
unitDir = \case
  Up -> (0, -1)
  Down -> (0, 1)
  Lef -> (-1, 0)
  Righ -> (1, 0)

type Heat = Int

type State = (Coord, Dir)

turns :: Dir -> [Dir]
turns = (`elem` [Up, Down]) >>> bool [Up, Down] [Lef, Righ]

cost :: M.Map Coord Int -> Coord -> Coord -> Maybe Heat
cost fls (x, y) (dx, dy) = traverse (fls M.!?) cs <&> sum
 where
  cs = do
    x' <- [min x dx .. max x dx]
    y' <- [min y dy .. max y dy]
    (x /= x' || y /= y') & guard
    pure (x', y')

neighbours :: M.Map Coord Int -> State -> [(State, Heat)]
neighbours fls ((x, y), d) = do
  m <- [1 .. 3]
  d' <- turns d
  d /= d' & guard
  let (dx, dy) = unitDir d' & bimap (* m) (* m)
  let c' = (x + dx, y + dy)
  h <- cost fls (x, y) c' & maybeToList
  pure ((c', d'), h)

part1 :: (MonadFail m) => M.Map Coord Int -> m (Heat, [State])
part1 fls = do
  let maxX = M.keys fls & fmap fst & maximum
  let maxY = M.keys fls & fmap snd & maximum
  dijkstraAssoc
    (neighbours fls)
    (\(c, _) -> c == (maxX, maxY))
    ((0, 0), Up)
    & maybe (fail "No solution found") pure

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= (intoCoords >>> part1)
    >>= (fst >>> print)
