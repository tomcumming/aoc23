module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as M
import System.IO (getContents')
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

readsFail :: (MonadFail m) => ReadP a -> String -> m a
readsFail f s =
  find ((== "") . snd) (P.readP_to_S f s)
    & maybe (fail $ "Could not reads: " <> s) (pure . fst)

readsRead :: (Read a) => ReadP a
readsRead = P.readS_to_P reads

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

main :: IO ()
main =
  getContents'
    >>= (lines >>> traverse (readsFail readsLine))
    >>= pure . filter (all (limitTo limits) . snd)
    >>= (fmap fst >>> sum >>> print)
