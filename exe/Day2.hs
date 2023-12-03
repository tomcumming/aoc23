module Main (main) where

import AOC.IO (readsFail, readsRead)
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import System.Environment (getArgs)
import System.IO (getContents')
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

readsLine :: ReadP (Int, [M.Map String Int])
readsLine = do
  void $ P.string "Game "
  game <- readsRead @Int
  void $ P.string ":"
  cnts <- P.sepBy1 colourCounts (P.string ";")
  pure (game, cnts)
 where
  colourCounts = do
    P.sepBy1 colourCount (P.string ",")
      <&> M.fromList
  colourCount = do
    void $ P.string " "
    cnt <- readsRead @Int
    void $ P.string " "
    col <- P.munch1 (`notElem` ",;")
    pure (col, cnt)

limits :: M.Map String Int
limits = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

limitTo :: M.Map String Int -> M.Map String Int -> Bool
limitTo ls xs = M.intersectionWith (>=) ls xs & and

smallest :: [M.Map String Int] -> M.Map String Int
smallest = M.unionsWith max

main :: IO ()
main =
  getArgs >>= \case
    [] -> ls >>= part1
    ["part2"] -> ls >>= part2
    _ -> fail "Unexpected args"
 where
  ls =
    getContents'
      >>= (lines >>> traverse (readsFail readsLine))
  part1 =
    filter (all (limitTo limits) . snd)
      >>> fmap fst
      >>> sum
      >>> print
  part2 =
    fmap (snd >>> smallest >>> product)
      >>> sum
      >>> print
