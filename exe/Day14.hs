module Main (main) where

import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Function ((&))
import Data.List qualified as L
import Data.Monoid (Sum (Sum), getSum)
import System.Environment (getArgs)
import System.IO (getContents')

parseInput :: String -> [String]
parseInput = lines

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x
  | f x == x = x
  | otherwise = fixedPoint f (f x)

rollNorth :: [String] -> [String]
rollNorth = L.transpose >>> fmap rollColumn >>> L.transpose

rollColumn :: String -> String
rollColumn = fixedPoint go
 where
  go = \case
    '.' : 'O' : xs -> 'O' : go ('.' : xs)
    c : xs -> c : go xs
    [] -> []

rotateCW :: [String] -> [String]
rotateCW = L.transpose >>> fmap reverse

rollCycle :: [String] -> [String]
rollCycle =
  rollNorth
    >>> rotateCW
    >>> rollNorth
    >>> rotateCW
    >>> rollNorth
    >>> rotateCW
    >>> rollNorth
    >>> rotateCW

calcLoad :: [String] -> Int
calcLoad =
  L.transpose
    >>> foldMap
      ( reverse
          >>> zipWith (\x c -> c == 'O' & bool 0 x) [1 ..]
          >>> foldMap Sum
      )
    >>> getSum

runBillions :: [[String]] -> [String] -> [String]
runBillions befores ss
  | Just idx <- L.elemIndex ss befores =
      let cycleLen = idx + 1
          left = 1_000_000_000 - length befores
       in reverse (take cycleLen befores) !! (left `mod` cycleLen)
  | otherwise = rollCycle ss & runBillions (ss : befores)

main :: IO ()
main = do
  f <-
    getArgs >>= \case
      [] -> pure rollNorth
      ["part2"] -> runBillions [] & pure
      _ -> fail "Unexpected args"
  getContents'
    >>= (parseInput >>> f >>> calcLoad >>> print)
