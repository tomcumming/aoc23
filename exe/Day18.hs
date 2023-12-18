module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad (guard, replicateM, void)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Set qualified as S
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

data Dir = L | R | U | D deriving (Eq, Ord, Show, Read)

parseLine :: P.ReadP ((Dir, Int), String)
parseLine = do
  d <- readsRead @Dir
  P.skipSpaces
  n <- readsRead @Int
  P.skipSpaces
  P.string "(#" & void
  h <- replicateM 6 P.get
  P.string ")" & void
  pure ((d, n), h)

parseInput :: (MonadFail m) => String -> m [((Dir, Int), String)]
parseInput = lines >>> traverse (readsFail parseLine)

type Coord = (Int, Int)

makeBoundary :: Coord -> [(Dir, Int)] -> S.Set Coord
makeBoundary (x, y) = \case
  [] -> mempty
  (dir, n) : ls ->
    let (e, cs) = case dir of
          U -> ((x, y - n), [(x, y - d) | d <- [1 .. n]])
          D -> ((x, y + n), [(x, y + d) | d <- [1 .. n]])
          L -> ((x - n, y), [(x - d, y) | d <- [1 .. n]])
          R -> ((x + n, y), [(x + d, y) | d <- [1 .. n]])
     in S.fromList cs <> makeBoundary e ls

bounds :: S.Set Coord -> ((Int, Int), (Int, Int))
bounds cs = ((minX, minY), (maxX, maxY))
 where
  minX = S.toList cs <&> fst & minimum
  maxX = S.toList cs <&> fst & maximum
  minY = S.toList cs <&> snd & minimum
  maxY = S.toList cs <&> snd & maximum

blankSpace :: S.Set Coord -> S.Set Coord
blankSpace cs =
  ( do
      x <- [minX .. maxX]
      y <- [minY .. maxY]
      let c = (x, y)
      S.notMember c cs & guard
      pure c
  )
    & S.fromList
 where
  ((minX, minY), (maxX, maxY)) = bounds cs

neighbours :: Coord -> S.Set Coord
neighbours (x, y) =
  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    & S.fromList

solveGroups ::
  S.Set (S.Set Coord) ->
  S.Set (S.Set Coord) ->
  S.Set (S.Set Coord)
solveGroups solved =
  S.minView >>> \case
    Nothing -> solved
    Just (s, ss) ->
      let ns = foldMap neighbours s
          (ds, us) = S.partition (S.disjoint ns) ss
       in if S.null us
            then solveGroups (S.insert s solved) ss
            else solveGroups solved (S.insert (S.unions (S.insert s us)) ds)

covered :: S.Set Coord -> S.Set Coord
covered bs =
  gs
    & S.filter (all inBounds)
    & fold
    & S.union bs
 where
  initialGs = foldMap (S.singleton >>> S.singleton) (blankSpace bs)
  gs = solveGroups mempty initialGs
  ((minX, minY), (maxX, maxY)) = bounds bs
  inBounds (x, y) = x `notElem` [minX, maxX] && y `notElem` [minY, maxY]

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= ( fmap fst
            >>> makeBoundary (0, 0)
            >>> covered
            >>> S.size
            >>> print
        )
