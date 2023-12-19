module Main (main) where

import AOC.IO (readFail)
import AOC.List (split)
import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Char (isAlpha)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Map qualified as M
import System.IO (getContents')

data Dst
  = Accept
  | Reject
  | Next String
  deriving (Show)

type Rule = (String, Ordering, Int, Dst)

parseDst :: (MonadFail m) => String -> m Dst
parseDst = \case
  'A' : _ -> pure Accept
  'R' : _ -> pure Reject
  s -> takeWhile isAlpha s & Next & pure

parseRule :: (MonadFail m) => String -> m Rule
parseRule =
  L.span isAlpha >>> \case
    (name, opC : rest)
      | [numStr, dstStr] <- split ':' rest -> do
          op <- case opC of
            '<' -> pure LT
            '>' -> pure GT
            _ -> fail "Unknown op"
          n <- readFail numStr
          dst <- parseDst dstStr
          pure (name, op, n, dst)
    _ -> fail "Unexpected rule"

parseWorkflow :: (MonadFail m) => String -> m (String, [Rule], Dst)
parseWorkflow =
  split '{' >>> \case
    [name, rulesStr] -> do
      let ruleParts = split ',' rulesStr
      rules <- traverse parseRule (take (length ruleParts - 1) ruleParts)
      def <- case drop (length ruleParts - 1) ruleParts of
        [defPart] -> parseDst defPart
        _ -> fail "Unexpected def parts"
      pure (name, rules, def)
    _ -> fail "Unexpected workflow"

parsePart :: (MonadFail m) => String -> m [(String, Int)]
parsePart =
  reverse
    >>> drop 1
    >>> reverse
    >>> drop 1
    >>> split ','
    >>> traverse parsePartPart
 where
  parsePartPart =
    split '=' >>> \case
      [name, numStr] -> readFail numStr <&> (name,)
      _ -> fail "Unexpected part part"

parseInput ::
  (MonadFail m) => String -> m ([(String, [Rule], Dst)], [[(String, Int)]])
parseInput =
  lines >>> split "" >>> \case
    [wf, ps] -> do
      w <- traverse parseWorkflow wf
      p <- traverse parsePart ps
      pure (w, p)
    _ -> fail "Unexpected input"

indexWfs :: [(String, [Rule], Dst)] -> M.Map String ([Rule], Dst)
indexWfs = fmap (\(name, rs, d) -> (name, (rs, d))) >>> M.fromList

part1 :: M.Map String ([Rule], Dst) -> [M.Map String Int] -> [M.Map String Int]
part1 wfs = filter (goWf "in")
 where
  goWf :: String -> M.Map String Int -> Bool
  goWf name xs =
    M.lookup name wfs & \case
      Nothing -> False -- error
      Just (rs, def) -> goRule xs def rs

  goRule :: M.Map String Int -> Dst -> [Rule] -> Bool
  goRule xs def = \case
    [] -> goDest xs def
    (pn, LT, n, dst) : _
      | Just m <- M.lookup pn xs
      , m < n ->
          goDest xs dst
    (pn, GT, n, dst) : _
      | Just m <- M.lookup pn xs
      , m > n ->
          goDest xs dst
    _ : rs' -> goRule xs def rs'

  goDest :: M.Map String Int -> Dst -> Bool
  goDest xs = \case
    Accept -> True
    Reject -> False
    Next name -> goWf name xs

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= ( bimap indexWfs (fmap M.fromList)
            >>> uncurry part1
            >>> fmap sum
            >>> sum
            >>> print
        )
