module SynTree where

import Control.Applicative
import Parsing (Parser, between, char, choice, just, letter, lexed, many1, onlyOne, space, spaces, string, token, tokens)

data SynTree o c = Leaf c | Not (SynTree o c) | Binary o (SynTree o c) (SynTree o c) deriving (Show, Read, Eq)

leafA :: SynTree Char Char
leafA = Leaf 'A'

notA :: SynTree Char Char
notA = Not leafA

notNotA :: SynTree Char Char
notNotA = Not notA

bin1 :: SynTree Char Char
bin1 = Binary 'v' leafA notA

display :: SynTree Char Char -> String
display (Leaf c) = [c]
display (Not t) = "~(" ++ display t ++ ")"
display (Binary o l r) = "(" ++ display l ++ ") " ++ [o] ++ " (" ++ display r ++ ")"

pretty :: SynTree Char Char -> String
pretty (Leaf c) = [c]
pretty (Not (Leaf c)) = "~" ++ [c]
pretty (Not t@Not {}) = "~" ++ pretty t
pretty (Not t) = "~(" ++ pretty t ++ ")"
pretty (Binary o l@(Binary {}) r@(Binary {})) = "(" ++ pretty l ++ ") " ++ [o] ++ " (" ++ pretty r ++ ")"
pretty (Binary o l@(Binary {}) r) = "(" ++ pretty l ++ ") " ++ [o] ++ " " ++ pretty r
pretty (Binary o l r@(Binary {})) = pretty l ++ " " ++ [o] ++ " (" ++ pretty r ++ ")"
pretty (Binary o l r) = pretty l ++ " " ++ [o] ++ " " ++ pretty r

parseTree :: Parser (SynTree Char Char)
parseTree =
  choice
    [ Leaf <$> lexed letter,
      Not <$> between (tokens "~(") (token ')') (lexed parseTree),
      (\l (o, r) -> Binary o l r)
        <$>
          between (token '(') (token ')') (lexed parseTree)
            <*> ((,) <$> lexed letter <*> between (token '(') (token ')') (lexed parseTree))
    ]

parsePrettyTree :: Parser (SynTree Char Char)
parsePrettyTree =
  onlyOne [parseBinary, parseNot, parseLeaf]
  where
    parseLeaf = Leaf <$> lexed letter
    parseNot = Not <$> (token '~' *> onlyOne [between (token '(') (token ')') parseBinary, parseNot, parseLeaf])
    parseBinary =
      (\l (o, r) -> Binary o l r)
        <$>
          onlyOne [between (token '(') (token ')') parseBinary, parseNot, parseLeaf]
            <*>
              ((,) <$> lexed letter <*> onlyOne [between (token '(') (token ')') parseBinary, parseNot, parseLeaf])
