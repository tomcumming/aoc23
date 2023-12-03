module AOC.IO (readFail, readsFail, readsRead) where

import Data.Foldable (find)
import Data.Function ((&))
import Text.ParserCombinators.ReadP qualified as P
import Text.Read (readEither)

readFail :: (MonadFail m, Read a) => String -> m a
readFail = either fail pure . readEither

readsFail :: (MonadFail m) => P.ReadP a -> String -> m a
readsFail f s =
  find ((== "") . snd) (P.readP_to_S f s)
    & maybe (fail $ "Could not reads: " <> s) (pure . fst)

readsRead :: (Read a) => P.ReadP a
readsRead = P.readS_to_P reads
