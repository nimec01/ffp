module Arithmetic where

import Control.Applicative ((<|>))
import Parsing (Parser, between, chainl1, just, lexed, number, onlyOne, succeed, token)

{-
  <expr> ::= <term> + <term> | <term>
  <term> ::= <factor> * <factor> | factor
  <factor> ::= digit+ | (<expr>)
-}

parseExpression :: Parser Int
parseExpression = chainl1 parseTerm parsePlus
  where
    parsePlus = (+) <$ token '+'

parseTerm :: Parser Int
parseTerm = chainl1 parseFactor parseMul
  where
    parseMul = (*) <$ token '*'

parseFactor :: Parser Int
parseFactor = lexed number <|> between (token '(') parseExpression (token ')')
