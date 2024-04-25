module Parsing where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isLetter, toLower, toUpper)
import Data.Foldable (find)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Prelude hiding (fail)

-- could be refined to support more than just string as an input type
-- uses the list of successes method; empty list <=> parser failed
newtype Parser output = Parser {runParser :: String -> [(String, output)]}

-- a parser that always succeeds with the provided value; does not consume any input
succeed :: a -> Parser a
succeed v = Parser $ \i -> [(i, v)]

-- read any char
any :: Parser Char
any = Parser any'
  where any' [] = []
        any' (x:xs) = [(xs,x)]

-- a parser that always fails; does not consume any input
fail :: Parser a
fail = Parser $ const []

-- reads nothing but succeeds
epsilon :: Parser ()
epsilon = Parser $ \i -> [(i, ())]

-- read end of line
eol :: Parser ()
eol = Parser eol'
  where eol' [] = [("",())]
        eol' _ = []

-- consumes a token if it satisfies the given predicate
satisfies :: (Char -> Bool) -> Parser Char
satisfies p = Parser x
  where
    x [] = []
    x (x' : xs) = [(xs, x') | p x']

-- apply p but discard results with remaining input strings
just :: Parser a -> Parser a
just (Parser p) = Parser $ \i -> filter (null . fst) $ p i

instance Functor Parser where
  fmap f (Parser x) = Parser $ \i -> [(xs, f v) | (xs, v) <- x i]

instance Applicative Parser where
  pure = succeed
  (Parser x) <*> (Parser y) = Parser $ \i ->
    [(xs2, f v2) | (xs1, f) <- x i, (xs2, v2) <- y xs1]

instance Alternative Parser where
  empty = fail
  (Parser x) <|> (Parser y) = Parser $ \i -> x i ++ y i

instance Monad Parser where
  (Parser x) >>= f = Parser $ \i -> concat [runParser (f v) xs | (xs, v) <- x i]

-- apply p one or more times
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` s = p `sepBy1` s <|> succeed []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` s = do
  x <- p
  xs <- many (s *> p)
  return (x : xs)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> succeed a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rec' a
  where
    rec' a =
      ( do
          f <- op
          b <- p
          rec' (f a b)
      )
        <|> succeed a

lexed :: Parser a -> Parser a
lexed p = spaces *> p

choice :: [Parser a] -> Parser a
choice = foldr (<|>) fail

onlyOne :: [Parser a] -> Parser a
onlyOne [] = fail
onlyOne ps = Parser $ \i ->
  let res = map (\(Parser p) -> p i) ps
   in maybe [] fst $ uncons $ filter (not . null) res

option :: Parser a -> Parser [a]
option p = (:[])<$> p <|> [] <$ epsilon

-- only accepts the specified character
char :: Char -> Parser Char
char e = satisfies (== e)

-- same as char but not case-sensitive
char' :: Char -> Parser Char
char' e = char (toUpper e) <|> char (toLower e)

-- accepts any letter
letter :: Parser Char
letter = satisfies isLetter

-- accepts any digit
digit :: Parser Char
digit = satisfies isDigit

-- accepts the specified sequence of characters
string :: String -> Parser String
string [] = succeed []
string (x : xs) = do
  char x
  xs' <- string xs
  return (x : xs')

-- accepts every char except the specified
notOf :: String -> Parser Char
notOf cs = satisfies (`notElem` cs)

-- accepts a space
space :: Parser Char
space = satisfies (== ' ')

-- accepts zero or more spaces
spaces :: Parser String
spaces = many space

token :: Char -> Parser Char
token e = spaces *> char e

-- accepts the specified sequence of characters with any leading space
tokens :: String -> Parser String
tokens s = spaces *> string s

between :: Parser a -> Parser b -> Parser c -> Parser b
between pStart pBody pEnd = pStart *> pBody <* pEnd

-- accepts a number
number :: Parser Int
number = read <$> many1 digit

-- accepts a number of lists enclosed in square brackets separated by ,
numList :: Parser [Int]
numList = between (spaces *> char '[') ((spaces *> number) `sepBy` (spaces *> char ',')) (spaces *> char ']')
