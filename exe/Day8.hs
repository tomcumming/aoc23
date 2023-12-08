module Main (main) where

import AOC.IO (readsFail)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

data Step = L | R deriving (Show)

parseInput :: P.ReadP ([Step], [(String, (String, String))])
parseInput = do
  steps <- P.many1 parseStep
  P.skipSpaces
  maps <- P.many1 parseMap
  P.skipSpaces
  pure (steps, maps)
 where
  parseStep =
    P.get >>= \case
      'L' -> pure L
      'R' -> pure R
      _ -> fail "Expected Step"
  parseMap = do
    P.skipSpaces
    src <- P.munch1 (not . isSpace)
    P.skipSpaces >> P.string "=" >> P.skipSpaces
    P.string "(" & void
    dstL <- P.munch1 isAlphaNum
    P.string "," >> P.skipSpaces
    dstR <- P.munch1 isAlphaNum
    P.string ")" & void
    pure (src, (dstL, dstR))

followPath :: M.Map String (String, String) -> String -> [Step] -> [String]
followPath m loc = \case
  L : ss | Just (l, _) <- m M.!? loc -> l : followPath m l ss
  R : ss | Just (_, r) <- m M.!? loc -> r : followPath m r ss
  _ -> []

main :: IO ()
main = do
  (ss, m) <- getContents' >>= readsFail parseInput
  let steps = followPath (M.fromList m) "AAA" (cycle ss)
  let lastStep = L.find (snd >>> (== "ZZZ")) ([1 :: Int ..] `zip` steps)
  print lastStep
