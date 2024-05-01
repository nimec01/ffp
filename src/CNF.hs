{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module CNF where
import Data.Map (Map, fromList, empty, insert, union, toList, (!))
import Parsing (Parser, spaces, satisfies, tokens, many1, sepBy1, lexed, letter, token, char, notOf, space, between, string, choice)
import Data.Char (isLetter, isUpper)
import Control.Applicative ((<|>))
import Text.Read (Lexeme(Symbol))
import qualified Data.Map as M (foldrWithKey)
import Data.List ((\\))
import Data.List.Extra (nubOrd)

data Symbol = Epsilon | Terminal String | NonTerminal String deriving (Show, Read, Eq, Ord)

isTerminal :: Symbol -> Bool
isTerminal (NonTerminal _) = False
isTerminal _ = True

data CNF = CNF {
  nonterminals :: [Symbol]
, terminals :: [Symbol]
, productions :: Map Symbol [[Symbol]]
, start :: Symbol
} deriving (Show, Read, Eq, Ord)

data Tree a = Node a [Tree a] deriving Show

type CNFTree = Tree Symbol

{-
  S -> aSa | bSb | ε
-}

matchingABGrammar :: String
matchingABGrammar = "<S> ::= a<S>a | b<S>b | ε ."

matchingABs :: CNF
matchingABs = CNF {
  nonterminals = [NonTerminal "S"]
, terminals = [Terminal "a", Terminal "b"]
, productions = fromList [(NonTerminal "S",[[Terminal "a", NonTerminal "S", Terminal "a"],[Terminal "b", NonTerminal "S", Terminal "b"], []])]
, start = NonTerminal "S"
}


parseNonTerminal :: Parser Symbol
parseNonTerminal = NonTerminal <$> between (char '<') (many1 letter) (char '>')

parseTerminal :: Parser Symbol
parseTerminal =
  Epsilon <$ (string "EPSILON" <|> string "ε")
  <|>
  Terminal <$> many1 letter
parseReplacement :: Parser [Symbol]
parseReplacement = space *> many1 (parseNonTerminal <|> parseTerminal) <* space

parseProductionRule :: Parser (Map Symbol [[Symbol]])
parseProductionRule = do
  left <- parseNonTerminal
  string " ::="
  replacements <- parseReplacement `sepBy1` char '|'
  char '.'
  return $ insert left replacements empty

parseCNF :: Parser CNF
parseCNF = do
  rules <- parseProductionRule `sepBy1` char '\n'
  let productions = foldl union empty rules
  let allSymbols = nubOrd $ M.foldrWithKey (\k v acc -> acc ++ [k] ++ concat v) [] productions
  let nonterminals = filter (not . isTerminal) allSymbols
  let terminals = allSymbols \\ nonterminals
  return $ CNF {
    nonterminals,
    terminals,
    productions,
    start = head nonterminals
  }


parseGrammar :: CNF -> Parser (Tree String)
parseGrammar CNF{..} = parseSym start
  where
    parseSym :: Symbol -> Parser (Tree String)
    parseSym (Terminal t) = Node t [] <$ string t
    parseSym s@(NonTerminal nt) = Node nt <$> parseRule (productions ! s)
    parseRule :: [[Symbol]] -> Parser [Tree String]
    parseRule = choice . map parseReplacement'
    parseReplacement' :: [Symbol] -> Parser [Tree String]
    parseReplacement' = mapM parseSym


asThenBs :: CNF
asThenBs = CNF {
  nonterminals = [NonTerminal "S",NonTerminal "A"],
  terminals = [Terminal "a",Terminal "b"],
  productions = fromList [
    (NonTerminal "A",[[Terminal "b",NonTerminal "A"],[Terminal "b"]]),
    (NonTerminal "S",[[Terminal "a",NonTerminal "S"],[Terminal "b"],[Terminal "b",NonTerminal "A"]])
    ],
  start = NonTerminal "S"
}
