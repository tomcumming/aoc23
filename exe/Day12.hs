module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Category ((>>>))
import Control.Monad.State (State, evalState, gets, modify)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map qualified as M
import System.Environment (getArgs)
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

data Spring = Operational | Damaged deriving (Eq, Ord, Show)

type CacheM = State (M.Map ([Int], [Maybe Spring]) Int)

runCached :: CacheM a -> a
runCached = flip evalState mempty

parseInput :: (MonadFail m) => String -> m [([Maybe Spring], [Int])]
parseInput = lines >>> traverse (readsFail parseLine)

parseLine :: P.ReadP ([Maybe Spring], [Int])
parseLine = do
  ss <- P.many1 spring <* P.string " "
  ns <- P.sepBy1 (readsRead @Int) (P.string ",")
  pure (ss, ns)
 where
  spring =
    P.get >>= \case
      '#' -> pure $ Just Damaged
      '.' -> pure $ Just Operational
      '?' -> pure Nothing
      _ -> fail "Not a spring"

skipOperational :: [Int] -> [Maybe Spring] -> CacheM Int
skipOperational ts ms = do
  cached <- gets $ M.lookup (ts, ms)
  case cached of
    Just n -> pure n
    Nothing -> do
      n <- case (ts, ms) of
        ([], []) -> pure 1
        (_, Just Damaged : _) -> damaged ts ms
        (_, Just Operational : ms') -> skipOperational ts ms'
        (_, Nothing : ms') -> (+) <$> skipOperational ts ms' <*> damaged ts ms
        (_, []) -> pure 0
      modify $ M.insert (ts, ms) n
      pure n

operational :: [Int] -> [Maybe Spring] -> CacheM Int
operational ts ms = case (ts, ms) of
  ([], []) -> pure 1
  (_, Just Operational : ms') -> skipOperational ts ms'
  (_, Nothing : ms') -> skipOperational ts ms'
  _ -> pure 0

damaged :: [Int] -> [Maybe Spring] -> CacheM Int
damaged ts ms = case (ts, ms) of
  ([], []) -> pure 1
  (1 : ts', m : ms') | maybe True (== Damaged) m -> operational ts' ms'
  (n : ts', m : ms') | maybe True (== Damaged) m -> damaged (pred n : ts') ms'
  _ -> pure 0

unfoldSprings :: ([Maybe Spring], [Int]) -> ([Maybe Spring], [Int])
unfoldSprings (ms, ns) =
  ( replicate 5 ms & intercalate [Nothing]
  , replicate 5 ns & concat
  )

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure id
      ["part2"] -> pure unfoldSprings
      _ -> fail "Unexpected args"
  getContents'
    >>= parseInput
    >>= ( fmap (f >>> uncurry (flip skipOperational) >>> runCached)
            >>> sum
            >>> print
        )
