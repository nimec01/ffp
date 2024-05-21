module Megaparsec.SynTree (mParseTree, mParsePrettyTree) where

import Megaparsec.Parsing
import SynTree (SynTree (..))
import Text.Megaparsec.Char (char, alphaNumChar, space, spaceChar)
import Text.Megaparsec (between, (<|>), skipMany, MonadParsec (try))

mParseLeaf :: MParser (SynTree Char Char)
mParseLeaf = Leaf <$> alphaNumChar

mParseNot :: MParser (SynTree Char Char)
mParseNot = Not <$> (char '~' *> between (char '(') (char ')') mParseLeaf)

mParseBinary :: MParser (SynTree Char Char)
mParseBinary =
 flip Binary <$>
    between (char '(') (char ')') mParseTree
    <*> (space *> alphaNumChar <* space)
    <*> between (char '(') (char ')') mParseTree

mParseTree :: MParser (SynTree Char Char)
mParseTree = mParseLeaf <|> mParseNot <|> mParseBinary


mParsePrettyLeaf :: MParser (SynTree Char Char)
mParsePrettyLeaf = mParseLeaf

mParsePrettyNot :: MParser (SynTree Char Char)
mParsePrettyNot =
  char '~' *> (Not <$> (mParsePrettyLeaf <|> mParsePrettyNot <|> between (char '(') (char ')') mParsePrettyBinary))

mParsePrettyBinary :: MParser (SynTree Char Char)
mParsePrettyBinary = do
  left <- between (char '(') (char ')') mParsePrettyBinary <|> mParsePrettyNot <|> mParsePrettyLeaf
  space
  op <- alphaNumChar
  space
  right <- between (char '(') (char ')') mParsePrettyBinary <|> mParsePrettyNot <|> mParsePrettyLeaf
  return $ Binary op left right

mParsePrettyTree :: MParser (SynTree Char Char)
mParsePrettyTree = try mParsePrettyBinary <|> mParsePrettyNot <|> mParsePrettyLeaf
