module Arithmetic where

import Control.Applicative ((<|>))
import Parsing (Parser, between, chainl1, just, lexed, nat, onlyOne, succeed, token, double, satisfies, char)
import Data.Char (isDigit)

{-
  <expr> ::= <expr> + <term> | <expr> - <term> | <term>
  <term> ::= <term> * <factor> | <term> / <factor> | <factor>
  <factor> ::= double | (<expr>)
-}

parseExpression :: Parser Double
parseExpression = chainl1 parseTerm parser
  where
    parser = (+) <$ token '+' <|> (-) <$ token '-'

parseTerm :: Parser Double
parseTerm = chainl1 parseFactor parser
  where
    parser = (*) <$ token '*' <|> (/) <$ token '/'

parseFactor :: Parser Double
parseFactor = lexed double <|> between (token '(') (token ')') parseExpression


parseDigitList :: Parser [Integer]
parseDigitList = digit' `chainl1` comma
  where digit' = (\d -> [read [d]]) <$> satisfies isDigit
        comma = (++) <$ char ','
