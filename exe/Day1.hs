module Main (main) where

import Control.Arrow (first, (>>>))
import Control.Monad (msum, (>=>))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.IO (getContents')
import Text.Read (readEither)

main :: IO ()
main = do
  ps <-
    getArgs
      <&> ( \case
              [] -> []
              ["part2"] -> ns
              _ -> error "unexpected args"
          )

  getContents'
    >>= day1 ps
    >>= print

readFail :: (MonadFail m, Read a) => String -> m a
readFail = either fail pure . readEither

day1 :: (MonadFail m) => [(String, Char)] -> String -> m Integer
day1 ps =
  lines
    >>> traverse (\ds -> sequence [firstDigit ps ds, lastDigit ps ds])
    >=> traverse readFail
    >>> fmap sum

firstDigit :: (MonadFail m) => [(String, Char)] -> String -> m Char
firstDigit ps = \case
  "" -> fail "Failed to find digit"
  c : _ | isDigit c -> pure c
  s | Just p <- msum (prefix s <$> ps) -> pure p
  _ : s -> firstDigit ps s
 where
  prefix :: String -> (String, Char) -> Maybe Char
  prefix s (p, c)
    | p `isPrefixOf` s = Just c
    | otherwise = Nothing

lastDigit :: (MonadFail m) => [([Char], Char)] -> [Char] -> m Char
lastDigit ps = firstDigit (first reverse <$> ps) . reverse

ns :: [(String, Char)]
ns =
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9')
  ]
