module Formula where
import Prelude hiding (any)
import Parsing (Parser(..), satisfies, string, char, choice, between, parens, token, chainl1, lexed, eol, runParser, any)
import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isAlpha)

{-
  <op> ::= ∧ | ∨ .
  <atom> ::= A | B | C | D | ... | Z .
  <formula> ::= <atom> | ¬ <formula> | <formula> <op> <formula> | (<formula>)
-}


data Op
  = And
  | Or deriving (Show,Eq)

data Formula
  = Binary Formula Op Formula
  | Negated Formula
  | Atom Char deriving (Show, Eq)

-- A ∧ (B ∨ C)
f :: Formula
f = Binary (Atom 'A') And (Binary (Atom 'B') Or (Atom 'C'))

alpha :: Parser Char
alpha = satisfies isAlpha

parseAtom :: Parser Formula
parseAtom = Atom <$> alpha

parseOp :: Parser Op
parseOp = const And <$> char '∧' <|> const Or <$> char '∨'

parseBracketsFormula :: Parser Formula
parseBracketsFormula = do
  token '('
  formula <- parseFormula
  token ')'
  return formula

lookup :: Parser a -> Parser b -> Parser b
lookup test (Parser p) = Parser $ \i -> if null (runParser (many any *> test) i) then [] else p i


parseFormula :: Parser Formula
parseFormula = choice
  [ parseAtom
  , Negated <$> (char '¬' *> parseBracketsFormula)
  , Formula.lookup (char '∧' <|> char '∨') (Binary <$> parseFormula <*> parseOp <*> parseFormula)
  , parseBracketsFormula
  ]

{-
  <atom> ::= A | B | C | D | ... | Z .
  <bracketFormula> ::= (<formula>).
  <disjunction> ::= <disjunction> ∨ <conjunction> | <conjunction>.
  <conjunction> ::= <conjunction> ∧ <term> | <term>.
  <term> ::= <atom> | <bracketFormula> | ¬<bracketFormula>.
  <formula> ::= <disjunction>.
-}

parseFormula' :: Parser Formula
parseFormula' = disjunction
  where
    atom = Atom <$> lexed alpha
    bracketFormula = between (token '(') (token ')') parseFormula'
    negated = Negated <$> (token '¬' *> bracketFormula)
    disjunction = conjunction `chainl1` ((`Binary` Or) <$ token '∨')
    conjunction = term `chainl1` ((`Binary` And) <$ token '∧')
    term = atom <|> bracketFormula <|> negated

parseFormula'' :: Parser Formula
parseFormula'' = atom <|> bracketFormula <|> negated <|> conjunction
  where
    atom = Atom <$> lexed alpha
    bracketFormula = between (token '(') (token ')') parseFormula'
    negated = Negated <$> (token '¬' *> bracketFormula)
    conjunction = (\x _ y -> Binary x And y) <$> conjunction <*> char '∧' <*> parseFormula'' <|> disjunction
    disjunction = (\x _ y -> Binary x Or y) <$> disjunction <*> char '∨' <*> parseFormula'' <|> parseFormula''
    -- conjunction = disjunction `chainl1` ((`Binary` Or) <$ lexed (string "or"))
    -- disjunction = (atom <|> bracketFormula <|> negated) `chainl1` ((`Binary` And) <$ lexed (string "and"))
