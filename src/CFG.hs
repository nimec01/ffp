{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module CFG where
import Data.Map (Map, fromList, empty, insert, union, toList, (!))
import Parsing (Parser, spaces, satisfies, tokens, many1, sepBy1, lexed, letter, token, char, notOf, space, between, string, choice, eol)
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

data CFG = CFG {
  nonterminals :: [Symbol]
, terminals :: [Symbol]
, productions :: Map Symbol [[Symbol]]
, start :: Symbol
} deriving (Show, Read, Eq, Ord)

data Tree a = Node a [Tree a] deriving Show

type CFGTree = Tree Symbol

{-
  S -> aSa | bSb | ε
-}

matchingABGrammar :: String
matchingABGrammar = "<S>::=a<S>a|b<S>b|ε."

matchingABs :: CFG
matchingABs = CFG {
  nonterminals = [NonTerminal "S"]
, terminals = [Terminal "a", Terminal "b"]
, productions = fromList [(NonTerminal "S",[[Terminal "a", NonTerminal "S", Terminal "a"],[Terminal "b", NonTerminal "S", Terminal "b"], []])]
, start = NonTerminal "S"
}


parseNonTerminal :: Parser Symbol
parseNonTerminal = NonTerminal <$> between (token '<') (token '>') (many1 letter)

parseTerminal :: Parser Symbol
parseTerminal =
  Epsilon <$ (tokens "EPSILON" <|> tokens "ε")
  <|>
  Terminal <$> many1 (lexed (satisfies (\x -> isLetter x && x /= 'ε')))

parseReplacement :: Parser [Symbol]
parseReplacement = many1 (parseNonTerminal <|> parseTerminal)

parseProductionRule :: Parser (Map Symbol [[Symbol]])
parseProductionRule = do
  left <- parseNonTerminal
  tokens "::="
  replacements <- parseReplacement `sepBy1` token '|'
  token '.'
  return $ insert left replacements empty

parseCFG :: Parser CFG
parseCFG = do
  rules <- parseProductionRule `sepBy1` char '\n'
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


parseGrammar :: CFG -> Parser (Tree String)
parseGrammar CFG{..} = parseSym start
  where
    parseSym :: Symbol -> Parser (Tree String)
    parseSym Epsilon = Node "" [] <$ eol
    parseSym (Terminal t) = Node t [] <$ string t
    parseSym s@(NonTerminal nt) = Node nt <$> parseRule (productions ! s)
    parseRule :: [[Symbol]] -> Parser [Tree String]
    parseRule = choice . map parseReplacement'
    parseReplacement' :: [Symbol] -> Parser [Tree String]
    parseReplacement' = mapM parseSym


asThenBs :: CFG
asThenBs = CFG {
  nonterminals = [NonTerminal "S",NonTerminal "B"],
  terminals = [Terminal "a",Epsilon,Terminal "b"],
  productions = fromList [(NonTerminal "B",[[Terminal "b"],[Terminal "b",NonTerminal "B"]]),(NonTerminal "S",[[Terminal "a",NonTerminal "S"],[Terminal "a",NonTerminal "B"]])],
  start = NonTerminal "S"
}
