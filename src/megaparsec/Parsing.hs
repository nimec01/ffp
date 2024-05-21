module Megaparsec.Parsing where

import Text.Megaparsec (Parsec, (<|>), many)
import Data.Void (Void)
import Text.Megaparsec.Char (spaceChar)

type MParser = Parsec Void String

mChainL :: MParser a -> MParser (a -> a -> a) -> a -> MParser a
mChainL p op a = p `mChainL1` op  <|> return a

mChainL1 :: MParser a -> MParser (a -> a -> a) -> MParser a
mChainL1 p op = do
  a <- p
  rec' a
  where rec' a = (do
          f <- op
          b <- p
          rec' (f a b)
          ) <|> return a

mLexed :: MParser a -> MParser a
mLexed p = p <* many spaceChar
