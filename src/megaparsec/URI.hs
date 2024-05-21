{-# LANGUAGE NamedFieldPuns #-}
module Megaparsec.URI (mParseURI) where

import Megaparsec.Parsing
import URI (URI (..))
import Text.Megaparsec (choice, MonadParsec (lookAhead, try, eof), some, optional, many, option, sepBy, noneOf, (<?>))
import Text.Megaparsec.Char (string, char, alphaNumChar, letterChar, digitChar, printChar, eol)
import Data.Map (Map(..), empty, insert, unions)
import Data.Maybe (fromMaybe)

mParseScheme :: MParser String
mParseScheme = choice [
  string "https",
  string "http",
  string "sftp"
  ]

mParseCredentials :: MParser (String, String)
mParseCredentials = (,) <$> (some alphaNumChar <* char ':') <*> some alphaNumChar

mParseParams :: MParser (Map String String)
mParseParams = unions <$> parseParam `sepBy` char ','
  where parseParam :: MParser (Map String String)
        parseParam = (\a b -> insert a b empty) <$> (some (noneOf "&=") <* char '=') <*> some (noneOf "&=")

mParseURI :: MParser URI
mParseURI = do
  scheme <- mParseScheme <?> "valid url scheme"
  string "://"
  credentials <- optional $ try (mParseCredentials <* char '@')
  domain <- (:) <$> letterChar <*> many (noneOf "?#=&/:")
  port <- optional (read <$> (char ':' *> some digitChar) :: MParser Int)
  directory <- optional ((:) <$> char '/' <*> many (noneOf "?#=&"))
  params <- optional (char '?' *> mParseParams)
  hash <- optional (char '#' *> some printChar)
  eof
  return $ URI {
    scheme
  , credentials
  , domain
  , port
  , directory = fromMaybe "/" directory
  , params = fromMaybe empty params
  , hash
  }
