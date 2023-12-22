module Main (main) where

import AOC.IO (readFail)
import AOC.List (split)
import Control.Category ((>>>))
import Control.Monad.State (State, evalState, gets, modify)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import System.Environment (getArgs)
import System.IO (getContents')

type Coord = (Int, Int, Int)

type Brick = (Coord, Coord)

parseInput :: forall m. (MonadFail m) => String -> m [Brick]
parseInput =
  lines
    >>> traverse
      ( split '~' >>> \case
          [a, b] -> (,) <$> parseCoord a <*> parseCoord b
          l -> "Can't parse line: " <> show l & fail
      )
 where
  parseCoord :: String -> m Coord
  parseCoord =
    split ',' >>> \case
      [x, y, z] -> do
        x' <- readFail x
        y' <- readFail y
        z' <- readFail z
        pure (x', y', z')
      c -> "can't parse coord: " <> show c & fail

overlapHoriz :: Brick -> Brick -> Bool
overlapHoriz ((x11, y11, _), (x12, y12, _)) ((x21, y21, _), (x22, y22, _)) =
  xOverlap && yOverlap
 where
  xOverlap = x12 >= x21 && x11 <= x22
  yOverlap = y12 >= y21 && y11 <= y22

under :: Brick -> Brick -> Bool
under b1@((_, _, z11), _) b2@(_, (_, _, z22)) = overlapHoriz b1 b2 && z11 > z22

topOf :: Brick -> Int
topOf ((_, _, _), (_, _, z2)) = z2

setFloor :: Brick -> Int -> Brick
setFloor ((x1, y1, z1), (x2, y2, z2)) flr =
  ((x1, y1, flr), (x2, y2, flr + (z2 - z1)))

settle' :: [Brick] -> Brick -> State (M.Map Brick Brick) Brick
settle' bs b = do
  existing <- gets (M.lookup b)
  case existing of
    Just b' -> pure b'
    Nothing -> do
      let us = filter (under b) bs
      us' <- traverse (settle' bs) us
      let top = case us' of
            [] -> 0
            _ -> us' <&> topOf & maximum
      let b' = setFloor b (succ top)
      modify (M.insert b b')
      pure b'

settle :: [Brick] -> [Brick]
settle bs = evalState (traverse (settle' bs) bs) mempty

withoutOne :: [a] -> [[a]]
withoutOne = go []
 where
  go xs = \case
    [] -> []
    (x : ys) -> (xs <> ys) : go (x : xs) ys

movers :: [Brick] -> [Int]
movers bs = do
  bs' <- withoutOne bs
  zipWith (==) bs' (settle bs')
    <&> bool 1 0
    & sum
    & pure

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure (filter (== 0) >>> length)
      ["part2"] -> pure sum
      _ -> fail "Unexpected args"
  getContents'
    >>= parseInput
    >>= (settle >>> movers >>> f >>> print)
