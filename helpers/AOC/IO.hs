module AOC.IO (readFail) where

import Text.Read (readEither)

readFail :: (MonadFail m, Read a) => String -> m a
readFail = either fail pure . readEither
