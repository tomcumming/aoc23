module Main (main) where

import AOC.IO (readFail, readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad (guard, void)
import Data.Function ((&))
import Data.List qualified as L
import Data.Traversable (forM)
import System.Environment (getArgs)
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

parseInput :: P.ReadP ([Int], [Int])
parseInput = do
  P.string "Time:" & void
  r1 <- P.many1 (P.skipSpaces >> readsRead @Int)
  P.skipSpaces
  P.string "Distance:" & void
  r2 <- P.many1 (P.skipSpaces >> readsRead @Int)
  P.skipSpaces
  pure (r1, r2)

times :: Int -> Int -> [Int]
times t d = do
  t' <- [0 .. t]
  let d' = (t - t') * t'
  d' > d & guard
  pure t'

part1 :: (MonadFail m) => ([Int], [Int]) -> m Int
part1 (r1, r2) = do
  xs <- forM (L.transpose [r1, r2]) $ \case
    [x, y] -> pure (x, y)
    _ -> fail "Unmatched pair"
  xs & fmap (uncurry times >>> length) & product & pure

part2 :: (MonadFail m) => ([Int], [Int]) -> m Int
part2 (r1, r2) = do
  let combiner xs = foldMap show xs & readFail @Int
  t <- combiner r1
  d <- combiner r2
  times t d & length & pure

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure part1
      ["part2"] -> pure part2
      _ -> fail "Unexpected args"
  getContents' >>= readsFail parseInput >>= f >>= print
