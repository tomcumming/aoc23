module Main (main) where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Set qualified as S
import System.IO (getContents')

type Coord = (Int, Int)

data Tile = Wall | Floor | Slope Coord deriving (Eq, Show)

parseInput :: forall m. (MonadFail m) => String -> m [[Tile]]
parseInput = lines >>> traverse (traverse parseTile)
 where
  parseTile :: Char -> m Tile
  parseTile = \case
    '#' -> pure Wall
    '.' -> pure Floor
    '>' -> Slope (1, 0) & pure
    '<' -> Slope (-1, 0) & pure
    '^' -> Slope (0, -1) & pure
    'v' -> Slope (0, 1) & pure
    c -> "Can't read tile: " <> show c & fail

type FloorPlan = M.Map Coord Tile

makeFloor :: [[Tile]] -> FloorPlan
makeFloor =
  zipWith (\y -> zipWith (\x -> ((x, y),)) [0 ..]) [0 ..]
    >>> concat
    >>> M.fromList

startAndEnd :: M.Map Coord Tile -> (Coord, Coord)
startAndEnd flr =
  ( M.toList flrs & filter (\((_, y), t) -> y == 0 && t == Floor) & head & fst
  , M.toList flrs & filter (\((_, y), t) -> y == ym && t == Floor) & head & fst
  )
 where
  flrs = M.filter (== Floor) flr
  ym = M.toList flrs <&> (\((_, y), _) -> y) & maximum

addc :: Coord -> Coord -> Coord
addc (x, y) = bimap (+ x) (+ y)

neighbours :: FloorPlan -> Coord -> S.Set Coord
neighbours flr c =
  ( do
      d <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
      let c' = addc c d
      let canSlide = case M.lookup c flr of
            Just (Slope d') -> d == d'
            _ -> True
      guard canSlide
      case M.lookup c' flr of
        Just Floor -> [c']
        Just (Slope _) -> [c']
        _ -> []
  )
    & S.fromList

type Solutions = S.Set (Coord, S.Set Coord)

stepPath :: FloorPlan -> Coord -> S.Set Coord -> S.Set (Coord, S.Set Coord)
stepPath flr c cs =
  neighbours flr c
    & (`S.difference` cs)
    & S.mapMonotonic (,S.insert c cs)

step :: FloorPlan -> Solutions -> Solutions
step flr = foldMap (uncurry (stepPath flr))

stepper :: Coord -> FloorPlan -> Solutions -> S.Set (Coord, S.Set Coord)
stepper end flr ss
  | S.null ss = mempty
  | otherwise =
      let (ends, ss') = S.partition (fst >>> (== end)) ss
       in ends <> stepper end flr (step flr ss')

part1 :: FloorPlan -> Int
part1 flr =
  let (start, end) = startAndEnd flr
   in stepper end flr (S.singleton (start, mempty))
        & S.toList
        & fmap (snd >>> S.size)
        & maximum

main :: IO ()
main = getContents' >>= parseInput >>= (makeFloor >>> part1 >>> print)
