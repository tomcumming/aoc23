module Main (main) where

import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Function ((&))
import Data.List qualified as L
import Data.Monoid (Sum (Sum), getSum)
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

calcLoad :: [String] -> Int
calcLoad =
  L.transpose
    >>> foldMap
      ( reverse
          >>> zipWith (\x c -> c == 'O' & bool 0 x) [1 ..]
          >>> foldMap Sum
      )
    >>> getSum

main :: IO ()
main =
  getContents'
    >>= ( parseInput
            >>> rollNorth
            >>> calcLoad
            >>> print
        )
