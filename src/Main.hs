module Main where
import Prelude hiding (sequence, fail)
import Data.Char (isAlpha, isDigit, isSpace)
import Control.Applicative (Alternative(..), (<**>))

data Op
  = And
  | Or deriving (Show,Eq)

data Formula
  = Binary Formula Op Formula
  | Negated Formula
  | Atom Char deriving (Show, Eq)

newtype Parser a = Parser (String -> [(String, a)])

val1 :: Formula
val1 = Binary (Atom 'A') And (Binary (Atom 'B') Or (Atom 'C'))

runParser :: Parser a -> String -> [(String, a)]
runParser (Parser p) = p

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = Parser x
  where
    x [] = []
    x (x' : xs) = [(xs, x') | p x']

-- alpha :: Parser Char
-- alpha = satisfies isAlpha

-- char :: Char -> Parser Char
-- char c = satisfies (== c)

alpha :: Parser Char
alpha = Parser p
  where p [] = []
        p (x:xs) = [(xs, x) | isAlpha x]

char :: Char -> Parser Char
char c = Parser p
  where p [] = []
        p (x:xs) = [(xs, x) | x == c]

pmap :: (a -> b) -> Parser a -> Parser b
pmap f (Parser p) = Parser $ \i -> [(xs, f v) | (xs, v) <- p i]

succeed :: a -> Parser a
succeed x = Parser $ \i -> [(i, x)]

fail :: Parser a
fail = Parser $ const []

eof :: Parser ()
eof = Parser $ \i -> [("", ()) | null i]

digit :: Parser Char
digit = satisfies isDigit

space :: Parser Char
space = satisfies isSpace

sequence :: Parser (a -> b) -> Parser a -> Parser b
sequence (Parser x) (Parser y) = Parser $ \i ->
    [(xs2, f v2) | (xs1, f) <- x i,(xs2, v2) <- y xs1]

combine :: (a->b->c) -> Parser a -> Parser b -> Parser c
-- combine f (Parser p1) (Parser p2) = Parser $ \i ->
--     [(xs2,f x1 x2) | (xs1,x1) <- p1 i, (xs2, x2) <- p2 xs1]
combine f p1 p2 = succeed f <*> p1 <*> p2

digitTuple :: Parser (Char, Char)
digitTuple = succeed (,) `sequence` digit `sequence` digit


instance Functor Parser where
  fmap = pmap

instance Applicative Parser where
  pure = succeed
  (<*>) = sequence

por :: Parser a -> Parser a -> Parser a
por (Parser x) (Parser y) = Parser $ \i -> x i ++ y i

instance Alternative Parser where
  empty = fail
  (<|>) = por

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <**> rest
  where rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id
-- chainl1 p op = p <|> p <**> op <*> chainl1 p op
-- chainl1 p op = p <|> chainl1 p op <**> op <*> p

parseDigitList :: Parser [Integer]
parseDigitList = parseSingleDigit `chainl1` comma
    where parseSingleDigit = (\d -> [read [d]]) <$> digit
          comma = const (++) <$> char ','

removeSpaces :: Parser a -> Parser a
removeSpaces p = p <* many space

token :: Char -> Parser Char
token c = removeSpaces (char c)


{-
  <atom> ::= A | B | C | D | ... | Z 
  <bracketFormula> ::= (<formula>)
  <negated> ::= ¬<bracketFormula>
  <disjunction> ::= <disjunction> ∨ <conjunction> | <conjunction>
  <conjunction> ::= <conjunction> ∧ <term> | <term>
  <term> ::= <atom> | <bracketFormula> | <negated>
  <formula> ::= <disjunction>
-}

parseFormula :: Parser Formula
parseFormula = disjunction
  where
    atom = Atom <$> removeSpaces alpha
    bracketFormula = (token '(' *> parseFormula) <* token ')'
    negated = Negated <$> (token '¬' *> bracketFormula)
    disjunction = conjunction `chainl1` ((`Binary` Or) <$ token '∨')
    conjunction = term `chainl1` ((`Binary` And) <$ token '∧')
    term = atom <|> bracketFormula <|> negated

parseFormula' :: Parser Formula -- 1st version
parseFormula' = disjunction
  where disjunction = (`Binary` Or) <$> (disjunction <* char '∨') <*> conjunction 
          <|> conjunction
        conjunction = (`Binary` And) <$> (conjunction <* char '∧') <*> term 
          <|> term
        atom = Atom <$> alpha
        bracketFormula = (char '(' *> parseFormula') <* char ')'
        negated =  Negated <$> (char '¬' *> bracketFormula)
        term = atom <|> bracketFormula <|> negated

parseFormula'' :: Parser Formula
parseFormula'' = disjunction
  where
    atom = Atom <$> alpha
    bracketFormula = (char '(' *> parseFormula'') <* char ')'
    negated = Negated <$> (char '¬' *> bracketFormula)
    disjunction = conjunction `chainl1` ((`Binary` Or) <$ char '∨')
    conjunction = term `chainl1` ((`Binary` And) <$ char '∧')
    term = atom <|> bracketFormula <|> negated

---

main :: IO ()
main = return ()
