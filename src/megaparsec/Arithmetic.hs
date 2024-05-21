module Megaparsec.Arithmetic where
import Megaparsec.Parsing (MParser, mChainL1)
import Control.Applicative (Alternative(some), optional)
import Text.Megaparsec.Char (digitChar, char)
import Text.Megaparsec (MonadParsec(try), (<|>), between)
import Data.Maybe (fromMaybe)

mParseDouble :: MParser Double
mParseDouble = do
  pre <- some digitChar
  extra <- optional $ try $ do
    char '.'
    post <- some digitChar
    return $ '.':post
  return $ read $ pre ++ fromMaybe "" extra


mParseExpression :: MParser Double
mParseExpression = mChainL1 mParseTerm parser
  where parser = (+) <$ char '+' <|> (-) <$ char '-'

mParseTerm :: MParser Double
mParseTerm = mChainL1 mParseFactor parser
  where parser = (*) <$ char '*' <|> (/) <$ char '/'

mParseFactor :: MParser Double
mParseFactor = mParseDouble <|> between (char '(') (char ')') mParseExpression


