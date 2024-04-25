{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module URI where
import Prelude hiding (any, foldr)
import Parsing (Parser, string, many1, notOf, nat, digit, option, char, any, chainl1, chainl)
import Control.Applicative ((<|>), Alternative (many))
import Data.Map (Map, empty, insert, union, toList, mapWithKey, foldrWithKey)
import qualified Data.Map as Map (null)
import Data.Functor (($>))
import Data.List (intercalate)

-- https://uni-due.de
-- sftp://username:password@example.com

data URI = URI {
  scheme :: String
, credentials :: Maybe (String, String)
, domain :: String
, port :: Maybe Int
, directory :: String
, params :: Map String String
, hash :: Maybe String
} deriving (Show, Eq)

fmidueURI :: URI
fmidueURI = URI {
  scheme = "https"
, credentials = Nothing
, domain = "uni-due.de"
, port = Nothing
, directory = "/fmi"
, params = empty
, hash = Nothing
}

prettyURI :: URI -> String
prettyURI URI{..} = concat [
    schemeString
  , credentialsString
  , domainString
  , portString
  , directoryString
  , paramsString
  , hashString
  ]
  where
    schemeString = scheme ++ "://"
    credentialsString = maybe "" (\(u,p) -> u ++ ":" ++ p ++ "@") credentials
    domainString = domain
    portString = maybe "" ((':':) . show) port
    directoryString = directory
    paramsString = if Map.null params
      then ""
      else "?" ++ intercalate "&" (foldrWithKey (\k v xs -> (k++"="++v):xs) [] params)
    hashString = maybe "" ('#':) hash

parseScheme :: Parser String
parseScheme =
  string "http" <|>
  string "https" <|>
  string "sftp"

parseCredentials :: Parser (String, String)
parseCredentials = do
  username <- many1 $ notOf " :@"
  char ':'
  password <- many1 $ notOf " :@"
  return (username, password)

parseDomain :: Parser String
parseDomain = many1 $ notOf "?#:/"

parseParams :: Parser (Map String String)
parseParams = chainl parseParam (char '&' $> flip union) empty
  where
    parseParam = do
      key <- many1 $ notOf "=&#"
      char '='
      value <- many1 $ notOf "&#"
      return $ insert key value empty


parseURI :: Parser URI
parseURI = do
  scheme <- parseScheme
  string "://"
  credentials' <- option (parseCredentials <* char '@')
  domain <- parseDomain
  port' <- option $ char ':' *> nat
  directory' <- option $ char '/' *> many1 (notOf "?#")
  params' <- option $ char '?' *> parseParams
  hash' <- option $ char '#' *> many1 any

  return $ URI {
    scheme
  , credentials = if null credentials' then Nothing else Just (head credentials')
  , domain
  , port = if null port' then Nothing else Just (head port')
  , directory = if null directory' then "/" else '/':head directory'
  , params = if null params' then empty else head params'
  , hash = if null hash' then Nothing else Just $ head hash'
  }

