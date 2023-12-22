module Main (main) where

import AOC.IO (readFail)
import AOC.List (split)
import Control.Category ((>>>))
import Control.Monad (guard)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
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

over :: Brick -> Brick -> Bool
over b1@((_, _, z11), _) b2@(_, (_, _, z22)) =
  overlapHoriz b1 b2
    && z11 == succ z22

topOf :: Brick -> Int
topOf ((_, _, _), (_, _, z2)) = z2

setFloor :: Brick -> Int -> Brick
setFloor ((x1, y1, z1), (x2, y2, z2)) flr =
  ((x1, y1, flr), (x2, y2, flr + (z2 - z1)))

settle :: [Brick] -> Brick -> State (M.Map Brick Brick) Brick
settle bs b = do
  existing <- gets (M.lookup b)
  case existing of
    Just b' -> pure b'
    Nothing -> do
      let us = filter (under b) bs
      us' <- traverse (settle bs) us
      let top = case us' of
            [] -> 0
            _ -> us' <&> topOf & maximum
      let b' = setFloor b (succ top)
      modify (M.insert b b')
      pure b'

disolvable :: [Brick] -> [Brick]
disolvable bs =
  let bs' = evalState (traverse (settle bs) bs) mempty
      os =
        ( do
            b1 <- bs'
            b2 <- bs'
            over b1 b2 & guard
            pure (b1, S.singleton b2)
        )
          & M.fromListWith (<>)
      us =
        M.toList os
          & foldMap (\(b1, bs2) -> S.toList bs2 <&> (,S.singleton b1))
          & M.fromListWith (<>)
   in do
        me <- bs'
        let overMe = M.lookup me us & maybe [] S.toList
        let actuallyDependant =
              filter
                ((`M.lookup` os) >>> fromMaybe mempty >>> S.size >>> (== 1))
                overMe
        null actuallyDependant & guard
        pure me

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= (disolvable >>> length >>> print)
