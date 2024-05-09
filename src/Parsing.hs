{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module Parsing where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isLetter, toLower, toUpper)
import Data.Foldable (find)
import Data.List (uncons, intercalate)
import Data.Maybe (fromJust)
import Data.Either (partitionEithers)
import Prelude hiding (fail)
import Data.Bifunctor (second)

-- could be refined to support more than just string as an input type
-- uses the list of successes method; empty list <=> parser failed
newtype Parser output = Parser {runParser :: String -> Either String [(String, output)]}

-- a parser that always succeeds with the provided value; does not consume any input
succeed :: a -> Parser a
succeed v = Parser $ \i -> Right [(i, v)]

-- read any char
any :: Parser Char
any = Parser any'
  where any' [] = Left "unexpected end of line"
        any' (x:xs) = Right [(xs,x)]

-- a parser that always fails; does not consume any input
fail :: String -> Parser a
fail x = Parser $ const $ Left x

-- reads nothing but succeeds
epsilon :: Parser ()
epsilon = Parser $ \i -> Right [(i, ())]

-- read end of line
eol :: Parser ()
eol = Parser eol'
  where eol' [] = Right [("",())]
        eol' _ = Left "expected end of line"

-- consumes a token if it satisfies the given predicate
satisfies :: (Char -> Bool) -> (Char -> String) -> Parser Char
satisfies p e = Parser x
  where
    x [] = Left "unexpected end of line"
    x (x' : xs) = case [(xs, x') | p x'] of
      [] -> Left $ e x'
      zs -> Right zs

-- apply p but discard results with remaining input strings
just :: Parser a -> Parser a
just (Parser p) = Parser $ \i -> case p i of
  Left x -> Left x
  Right x -> let xs = filter (null . fst) x in if null xs then Left "" else Right xs


instance Functor Parser where
  fmap f (Parser x) = Parser $ \i -> fmap (\y -> [(xs, f v) | (xs, v) <- y]) (x i)
    -- case x i of
    -- Left y -> Left y
    -- Right y -> Right [(xs, f v) | (xs, v) <- y]

instance Applicative Parser where
  pure = succeed
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser x) <*> (Parser y) = Parser $ \i ->
    case x i of
      Left a -> Left a
      Right xs -> convert na
        where
          na = flip map xs $ \(xs1, f) -> case y xs1 of
            Left b -> Left b
            Right u -> Right $ flip map u $ second f
          convert :: [Either String [(String, a)]] -> Either String [(String, a)]
          convert [] = Left "applicative error"
          convert zs = let (ls, rs) = partitionEithers zs in
            if null rs then Left $ intercalate ";" ls
                        else Right $ concat rs
    -- [(xs2, f v2) | (xs1, f) <- x i, (xs2, v2) <- y xs1]

instance Alternative Parser where
  empty = fail "failed"
  (Parser x) <|> (Parser y) = Parser $ \i -> combine (x i) (y i)
    where combine (Right a) (Right b) = Right $ a ++ b
          combine (Left _) (Right b) = Right b
          combine (Right a) (Left _) = Right a
          combine (Left a) (Left b) = Left $ a ++ "; " ++ b

instance Monad Parser where
  (Parser x) >>= f = Parser $ \i ->
    case x i of
      Left a -> Left a
      Right bs -> let ns = map (\(xs, v) -> runParser (f v) xs) bs in
        if null ns then Left "monad error"
                   else convert ns
    where
      convert :: [Either String [(String, a)]] -> Either String [(String, a)]
      convert [] = Left "applicative error"
      convert zs = let (ls, rs) = partitionEithers zs in
        if null rs then Left $ intercalate ";" ls
                    else Right $ concat rs
    -- concat [runParser (f v) xs | (xs, v) <- x i]

-- apply p one or more times
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` s = p `sepBy1` s <|> succeed []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` s = (:) <$> p <*> many (s *> p)

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
choice = foldr (<|>) (fail "choice failed")

onlyOne :: [Parser a] -> Parser a
onlyOne [] = fail "no choices"
onlyOne ps = Parser $ \i ->
  let res = map (\(Parser p) -> p i) ps
   in maybe (Left "no choice succeeded") fst $ uncons $ filter (not . null) res

option :: Parser a -> Parser [a]
option p = (:[])<$> p <|> [] <$ epsilon

-- only accepts the specified character
char :: Char -> Parser Char
char e = satisfies (== e) $ \c -> "unexpected " ++ [c] ++ ", expected " ++ [e]

-- same as char but not case-sensitive
char' :: Char -> Parser Char
char' e = char (toUpper e) <|> char (toLower e)

-- accepts any letter
letter :: Parser Char
letter = satisfies isLetter $ \c -> "expected a letter but read " ++ [c]

-- accepts any digit
digit :: Parser Char
digit = satisfies isDigit $ \c -> "expected a digit but read " ++ [c]

-- accepts the specified sequence of characters
string :: String -> Parser String
string [] = succeed []
string (x:xs) = (x:) <$> (char x *> string xs)

-- accepts every char except the specified
notOf :: String -> Parser Char
notOf cs = satisfies (`notElem` cs) $ \c -> "unexpected " ++ [c]

-- accepts a space
space :: Parser Char
space = satisfies (== ' ') $ const "missing space"

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
numList = between (spaces *> char '[') ((spaces *> nat) `sepBy` (spaces *> char ',')) (spaces *> char ']')
