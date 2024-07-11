{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Parsing where


import Control.Applicative (Alternative (..), (<**>))
import Data.Char (isDigit, isLetter, toLower, toUpper, isAlpha)
import Data.Foldable (find)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Prelude hiding (fail, sequence)

-- could be refined to support more than just string as an input type
-- uses the list of successes method; empty list <=> parser failed
-- newtype Parser output = Parser {runParser :: String -> [(String, output)]}
newtype Parser a = Parser (String -> [(String, a)])

runParser :: Parser a -> String -> [(String, a)]
runParser (Parser p) = p

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
-- eol = Parser $ \i -> [("",()) | null i]
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

pmap :: (a -> b) -> Parser a -> Parser b
pmap f (Parser x) = Parser $ \i -> [(xs, f v) | (xs, v) <- x i]

sequence :: Parser (a -> b) -> Parser a -> Parser b
(Parser x) `sequence` (Parser y) = Parser $ \i ->
    [(xs2, f v2) | (xs1, f) <- x i, (xs2, v2) <- y xs1]

instance Functor Parser where
  fmap f (Parser x) = Parser $ \i -> [(xs, f v) | (xs, v) <- x i]

instance Applicative Parser where
  pure = succeed
  (Parser x) <*> (Parser y) = Parser $ \i ->
    [(xs2, f v2) | (xs1, f) <- x i, (xs2, v2) <- y xs1]

instance Alternative Parser where
  empty = fail
  (Parser x) <|> (Parser y) = Parser $ \i -> x i ++ y i

infixl 3 <|!>
(<|!>) :: Parser a -> Parser a -> Parser a
(Parser x) <|!> (Parser y) = Parser $ \i -> let x' = x i in
  if null x' then y i
  else x'

instance Monad Parser where
  (Parser x) >>= f = Parser $ \i -> concat [runParser (f v) xs | (xs, v) <- x i]

-- apply p one or more times
many1 :: Parser a -> Parser [a]
-- many1 p = (:) <$> p <*> many p
many1 = some

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` s = p `sepBy1` s <|> succeed []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` s = (:) <$> p <*> many (s *> p)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> succeed a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <**> rest
  where rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

chainl1' :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1' p op = p <|> p <**> op <*> chainl1' p op

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = p `chainr1` op <|> succeed a

chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = rec'
  where
    rec' = do
      x <- p
      rec'' x
    rec'' a =
      (do
        f <- op
        f a <$> rec'
      ) <|> succeed a


lexed :: Parser a -> Parser a
lexed p = spaces *> p

choice :: [Parser a] -> Parser a
choice = foldr (<|>) fail

choice' :: [Parser a] -> Parser a
choice' = foldr (<|!>) fail

onlyOne :: [Parser a] -> Parser a
onlyOne [] = fail
onlyOne ps = Parser $ \i ->
  let res = map (\(Parser p) -> p i) ps
   in maybe [] fst $ uncons $ filter (not . null) res

option :: Parser a -> Parser [a]
option p = (:[])<$> p <|> [] <$ epsilon


anyChar :: Parser Char
anyChar = Parser p
  where p [] = []
        p (x:xs) = [(xs, x) | isAlpha x]

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
string (x:xs) = (x:) <$> (char x *> string xs)

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

between :: Parser a -> Parser b -> Parser c -> Parser c
between pStart pEnd pBody = pStart *> pBody <* pEnd

parens :: Parser a -> Parser a
parens = between (token '(') (token ')')

-- accepts a natural number (including 0)
nat :: Parser Int
nat = read <$> many1 digit

int :: Parser Int
int = (0 -) <$> (char '-' *> nat) <|> nat

double :: Parser Double
double =
  (\m s -> convertSig m * read s)
    <$> option (char '-') <*> ((\x y -> x++"."++y) <$> many1 digit <*>(char '.' *> many1 digit))
  <|> (fromIntegral <$> int)
  where convertSig [] = 1
        convertSig _ = -1

-- accepts a number of lists enclosed in square brackets separated by ,
numList :: Parser [Int]
numList = between (spaces *> char '[') (spaces *> char ']') ((spaces *> nat) `sepBy` (spaces *> char ','))




digitTuple :: Parser (Char, Char)
digitTuple = ((\x y -> (x,y)) `pmap` digit) `sequence` digit
