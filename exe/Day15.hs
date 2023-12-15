module Main (main) where

import AOC.IO (readFail)
import AOC.List (split)
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Char (isSpace, ord)
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..), getSum)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (getContents')

parseInput :: String -> [String]
parseInput = takeWhile (isSpace >>> not) >>> split ','

hash :: String -> Word8
hash = go 0
 where
  go n = \case
    [] -> n
    x : xs ->
      let n' =
            n
              & (+ fromIntegral (ord x))
              & (* 17)
       in go n' xs

parseInst :: String -> IO (String, Maybe Word8)
parseInst s
  | [lbl, n] <- split '=' s = readFail n >>= (Just >>> (lbl,) >>> pure)
  | "-" `L.isSuffixOf` s = pure (take (length s - 1) s, Nothing)
  | otherwise = "Can't parse instruction: " <> s & fail

insertLens ::
  String ->
  Word8 ->
  M.Map Word8 [(String, Word8)] ->
  M.Map Word8 [(String, Word8)]
insertLens lbl fl bs =
  let existing = M.lookup (hash lbl) bs & fromMaybe []
   in M.insert (hash lbl) (go existing) bs
 where
  go = \case
    (lbl', _) : ls | lbl == lbl' -> (lbl, fl) : ls
    l : ls -> l : go ls
    [] -> [(lbl, fl)]

removeLens ::
  String ->
  M.Map Word8 [(String, Word8)] ->
  M.Map Word8 [(String, Word8)]
removeLens lbl = M.adjust (filter (fst >>> (/= lbl))) (hash lbl)

part2 :: M.Map Word8 [(String, Word8)] -> [(String, Maybe Word8)] -> IO ()
part2 bs = \case
  [] ->
    M.foldMapWithKey (\idx -> sumBox idx >>> Sum) bs
      & getSum
      & print
  (lbl, Nothing) : insts -> part2 (removeLens lbl bs) insts
  (lbl, Just fl) : insts -> part2 (insertLens lbl fl bs) insts
 where
  sumBox :: Word8 -> [(String, Word8)] -> Integer
  sumBox idx ls =
    (fromIntegral idx + 1)
      * sum (zipWith (\n (_, fl) -> n * fromIntegral fl) [1 ..] ls)

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> (fmap (hash >>> fromIntegral) >>> sum >>> print @Integer) & pure
      ["part2"] -> (traverse parseInst >=> part2 mempty) & pure
      _ -> fail "Unexpected args"
  getContents' >>= (parseInput >>> f)
