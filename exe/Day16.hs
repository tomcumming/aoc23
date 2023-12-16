module Main (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import System.IO (getContents')

data Tile = Floor | SplitV | SplitH | RightUp | RightDown deriving (Show)

parseInput :: (MonadFail m) => String -> m [[Tile]]
parseInput = lines >>> traverse (traverse parseTile)
 where
  parseTile = \case
    '.' -> pure Floor
    '-' -> pure SplitH
    '|' -> pure SplitV
    '/' -> pure RightUp
    '\\' -> pure RightDown
    c -> "Not a tile: " <> show c & fail

type Coord = (Int, Int)

floorGrid :: [[Tile]] -> M.Map Coord Tile
floorGrid =
  zipWith (\y -> zipWith (\x t -> ((x, y), t)) [0 ..]) [0 ..]
    >>> concat
    >>> M.fromList

data Dir = Left_ | Right_ | Up | Down deriving (Eq, Ord, Show)

advanceCoord :: Coord -> Dir -> Coord
advanceCoord (x, y) = \case
  Left_ -> (x - 1, y)
  Right_ -> (x + 1, y)
  Up -> (x, y - 1)
  Down -> (x, y + 1)

rightUp :: Dir -> Dir
rightUp = \case Left_ -> Down; Up -> Right_; Down -> Left_; Right_ -> Up

rightDown :: Dir -> Dir
rightDown = \case Left_ -> Up; Up -> Left_; Down -> Right_; Right_ -> Down

advance :: M.Map Coord Tile -> Coord -> Dir -> S.Set (Coord, Dir)
advance fls c l = case M.lookup c fls of
  Just RightUp -> let l' = rightUp l in S.singleton (advanceCoord c l', l')
  Just RightDown -> let l' = rightDown l in S.singleton (advanceCoord c l', l')
  Just SplitV
    | l `elem` [Left_, Right_] ->
        S.fromList
          [(advanceCoord c Up, Up), (advanceCoord c Down, Down)]
  Just SplitH
    | l `elem` [Up, Down] ->
        S.fromList
          [(advanceCoord c Left_, Left_), (advanceCoord c Right_, Right_)]
  _ -> S.singleton (advanceCoord c l, l)

pruneBeam ::
  M.Map Coord Tile ->
  S.Set (Coord, Dir) ->
  (Coord, Dir) ->
  S.Set (Coord, Dir)
pruneBeam fls vs cd@(c, _)
  | M.notMember c fls = mempty
  | S.member cd vs = mempty
  | otherwise = S.singleton cd

step ::
  M.Map Coord Tile ->
  S.Set (Coord, Dir) ->
  S.Set (Coord, Dir) ->
  S.Set (Coord, Dir)
step fls vs =
  foldMap (advance fls & uncurry)
    >>> foldMap (pruneBeam fls vs)

stepper ::
  M.Map Coord Tile ->
  S.Set (Coord, Dir) ->
  S.Set (Coord, Dir) ->
  S.Set (Coord, Dir)
stepper fls vs cs = case step fls vs cs of
  cs' | S.null cs' -> vs <> cs
  cs' -> stepper fls (vs `S.union` cs) cs'

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= (floorGrid >>> go >>> S.size >>> print)
 where
  go fls =
    stepper fls mempty (S.singleton ((0, 0), Right_))
      & foldMap (fst >>> S.singleton)
