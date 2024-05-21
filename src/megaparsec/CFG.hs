{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Megaparsec.CFG (mParseCFG, mParseGrammar) where
import Megaparsec.Parsing (MParser, mLexed)
import CFG (Tree(..), CFG(..), Symbol(..), isTerminal)
import Text.Megaparsec (MonadParsec(eof, try), choice, between, some, (<|>), sepBy1, satisfy)
import Text.Megaparsec.Char (string, letterChar, char, newline)
import Data.Map ((!), Map, empty, insert, union)
import qualified Data.Map as M (foldrWithKey)
import Data.List ((\\))
import Data.List.Extra (nubOrd)
import Data.Char (isLetter)

mParseNonTerminal :: MParser Symbol
mParseNonTerminal = NonTerminal <$> between (mLexed (char '<')) (mLexed (char '>')) (some letterChar)

mParseTerminal :: MParser Symbol
mParseTerminal =
  try (Epsilon <$ (try (string "EPSILON") <|> string "ε"))
  <|>
  Terminal <$> some (mLexed (satisfy (\x -> isLetter x && x /= 'ε')))

mParseReplacement :: MParser [Symbol]
mParseReplacement = some (try mParseNonTerminal <|> mParseTerminal)

mParseProductionRule :: MParser (Map Symbol [[Symbol]])
mParseProductionRule = do
  left <- mParseNonTerminal
  mLexed $ string "::="
  replacements <- mParseReplacement `sepBy1` mLexed (char '|')
  mLexed $ char '.'
  return $ insert left replacements empty

mParseCFG :: MParser CFG
mParseCFG = do
  rules <- mParseProductionRule `sepBy1` newline
  let productions = foldl union empty rules
  let allSymbols = nubOrd $ M.foldrWithKey (\k v acc -> acc ++ [k] ++ concat v) [] productions
  let nonterminals = filter (not . isTerminal) allSymbols
  let terminals = allSymbols \\ nonterminals
  return $ CFG {
    nonterminals,
    terminals,
    productions,
    start = head nonterminals
  }

mParseGrammar :: CFG -> MParser (Tree String)
mParseGrammar CFG{..} = mParseSym start
  where
    mParseSym :: Symbol -> MParser (Tree String)
    mParseSym Epsilon = Node "" [] <$ return ()
    mParseSym (Terminal t) = Node t [] <$ string t
    mParseSym s@(NonTerminal nt) = Node nt <$> mParseRule (productions ! s)
    mParseRule :: [[Symbol]] -> MParser [Tree String]
    mParseRule = choice . map (try . mParseReplacement')
    mParseReplacement' :: [Symbol] -> MParser [Tree String]
    mParseReplacement' = mapM mParseSym
