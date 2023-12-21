module Main (main) where

import AOC.IO (readsFail)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import System.IO (getContents')
import Text.ParserCombinators.ReadP qualified as P

data Pulse = Low | High deriving (Eq, Show)

data Module
  = BroadCast
  | FlipFlop Bool
  | Conj (M.Map String Pulse)
  deriving (Show)

parseInput :: (MonadFail m) => String -> m [(String, Module, S.Set String)]
parseInput = lines >>> traverse (readsFail parseLine)
 where
  parseLine :: P.ReadP (String, Module, S.Set String)
  parseLine = do
    name <- P.munch1 (isSpace >>> not)
    (t, n) <- case name of
      "broadcaster" -> pure (name, BroadCast)
      '&' : name' -> pure (name', Conj mempty)
      '%' : name' -> pure (name', FlipFlop False)
      _ -> "Unexpected name: " <> name & fail
    P.string " -> " & void
    dsts <- P.sepBy1 (P.munch1 (/= ',')) (P.string ", ")
    pure (t, n, S.fromList dsts)

type Graph = M.Map String (Module, S.Set String)

initialGraph ::
  [(String, Module, S.Set String)] ->
  Graph
initialGraph ls =
  fmap
    ( \(name, m, cs) -> case m of
        Conj _
          | ps <- M.lookup name parentMap & fromMaybe mempty ->
              (name, (foldMap (`M.singleton` Low) ps & Conj, cs))
        _ -> (name, (m, cs))
    )
    ls
    & M.fromList
 where
  parentMap =
    foldMap (\(n, _, cs) -> foldMap (\c -> [(c, S.singleton n)]) cs) ls
      & M.fromListWith (<>)

-- initialize Conj

step ::
  Graph ->
  String ->
  String ->
  Pulse ->
  (Graph, M.Map String Pulse)
step g src dst p = case M.lookup dst g of
  Nothing -> (g, mempty)
  Just (BroadCast, cs) -> (g, foldMap (`M.singleton` p) cs)
  Just (FlipFlop x, cs) -> case (x, p) of
    (_, High) -> (g, mempty)
    (True, Low) ->
      ( M.insert dst (FlipFlop False, cs) g
      , foldMap (`M.singleton` Low) cs
      )
    (False, Low) ->
      ( M.insert dst (FlipFlop True, cs) g
      , foldMap (`M.singleton` High) cs
      )
  Just (Conj ms, cs) -> case M.insert src p ms of
    ms' ->
      ( M.insert dst (Conj ms', cs) g
      , foldMap (`M.singleton` bool High Low (all (== High) ms')) cs
      )

addPulses :: (Foldable t) => (Int, Int) -> t Pulse -> (Int, Int)
addPulses =
  foldl'
    ( \(highs, lows) -> \case
        High -> (succ highs, lows)
        Low -> (highs, succ lows)
    )

oneRound ::
  Graph -> (Int, Int) -> [(String, String, Pulse)] -> (Graph, (Int, Int))
oneRound g c = \case
  [] -> (g, c)
  (src, dst, p) : rest ->
    let (g', ps) = step g src dst p
        addedPs = M.foldMapWithKey (\dst' p' -> [(dst, dst', p')]) ps
     in oneRound g' (addPulses c ps) (rest <> addedPs)

stepper :: Graph -> (Int, Int) -> Int -> (Int, Int)
stepper g (h, l) = \case
  0 -> (h, l)
  n ->
    let (g', (h', l')) = oneRound g (0, 1) [("broadcaster", "broadcaster", Low)]
     in stepper g' (h + h', l + l') (pred n)

main :: IO ()
main =
  getContents'
    >>= parseInput
    >>= ( initialGraph
            >>> (\g -> stepper g (0, 0) 1000)
            >>> uncurry (*)
            >>> print
        )
