{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
module ParsingSpec where

import Control.Applicative (Alternative (many))
import Data.Char (isDigit, isLetter, toLower)
import Data.List.Extra (intersperse, notNull, replace)
import Parsing (Parser (runParser), char, char', digit, epsilon, fail, letter, many1, numList, nat, satisfies, sepBy1, space, spaces, string, succeed, token, tokens, notOf, option)
import Test.Hspec
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat, vectorOf)
import Prelude hiding (fail)

randomInput :: Gen String
randomInput = randomInput' 10

randomInput' :: Int -> Gen String
randomInput' x = vectorOf x (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789^°!§$%&/()=?*#,.-_<>@ ")

spec :: Spec
spec = do
  describe "succeed" $ do
    it "always return the right value (Int)" $
      forAll (choose (0, maxBound :: Int)) $ \i ->
        forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
          runParser (succeed (show i) :: Parser String) s == [(s, show i)]
    it "always return the right value (String)" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \i ->
        forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
          runParser (succeed i :: Parser String) s == [(s, i)]
  describe "fail" $ do
    it "always fails" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        null (runParser fail s)
  describe "epsilon" $ do
    it "always succeeds without reading input" $ do
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        runParser epsilon s == [(s, ())]
  describe "satisfies" $ do
    it "fails on empty input" $
      null (runParser (satisfies (const True)) "")
    it "always fails if the predicate is always False" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        null (runParser (satisfies (const False)) s)
    it "always succeeds if the predicate is always True" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        runParser (satisfies (const True)) s == [(tail s, head s)]
  describe "just" $ do
    xit "tests" True
  describe "Functor Parser" $ do
    it "Functors must preserve identity morphisms" $
      forAll (randomInput `suchThat` (isDigit . head)) $ \s ->
        runParser digit s == runParser (fmap id digit) s
    it "Functors preserve composition of morphisms" $
      forAll (randomInput `suchThat` (isDigit . head)) $ \s ->
        runParser (fmap ((< 10) . (* 3)) nat) s == runParser ((fmap (< 10) . fmap (* 3)) nat) s
  describe "Applicative Parser" $ do
    xit "tests" True
  describe "Monad Parser" $ do
    xit "tests" True
  describe "many" $ do
    it "succeeds" $
      forAll (choose (0, 100 :: Int)) $ \x ->
        runParser (many letter) (replicate x 'a') == [(replicate a 'a', replicate (x - a) 'a') | a <- [0 .. x]]
  describe "many1" $ do
    it "fails on empty input" $
      null (runParser (many1 digit) "")
    it "succeeds" $
      forAll (choose (1, 100 :: Int)) $ \x ->
        runParser (many1 letter) (replicate x 'a') == [(replicate a 'a', replicate (x - a) 'a') | a <- [0 .. x - 1]]
  describe "sepBy" $ do
    xit "tests" True
  describe "sepBy1" $ do
    it "fails on empty input" $
      null (runParser (sepBy1 digit (char ',')) "")
    xit "succeeds" $
      forAll (randomInput `suchThat` \s' -> all isLetter s' && notNull s') $ \s ->
        let s' = intersperse ',' s
            l = length s
         in runParser (sepBy1 letter (char ',')) s' == [(drop a s', take (l `div` 2) s) | a <- [l, l - 2 .. 0]]
  describe "chainl" $ do
    xit "tests" True
  describe "chainl1" $ do
    xit "tests" True
  describe "option" $ do
    it "succeeds on empty input" $
      runParser (option letter) "" == [("","")]
    it "succeeds when parser can be applied" $
      forAll (randomInput `suchThat` (isLetter . head)) $ \i ->
        runParser (option letter) i == [(tail i, [head i]), (i, "")]
  describe "char" $ do
    it "fails on empty input" $
      null (runParser (char 'x') "")
    it "succeeds when char matches" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        runParser (char (head s)) s == [(tail s, head s)]
    it "fails when char does not match" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        forAll (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `suchThat` (/= head s)) $ \x ->
          null (runParser (char x) s)
    it "is case sensitive" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        null (runParser (char (toLower (head s))) s)
  describe "char'" $ do
    it "fails on empty input" $
      null (runParser (char' 'x') "")
    it "succeeds when char matches" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        forAll (elements [True, False]) $ \f ->
          let el = (if f then toLower else id) (head s)
           in runParser (char' el) s == [(tail s, head s)]
    it "fails when char does not match" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        forAll (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `suchThat` (/= head s)) $ \x ->
          forAll (elements [True, False]) $ \f ->
            let el = (if f then toLower else id) x
             in null (runParser (char' el) s)
  describe "letter" $ do
    it "fails on empty input" $
      null (runParser letter "")
    it "succeeds when first character is a letter" $
      forAll (sublistOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \s ->
        runParser letter s == [(tail s, head s)]
    it "fails when first character is not a letter" $
      forAll (vectorOf 10 (elements "0123456789°^!§$%&/()=?")) $ \s ->
        null (runParser letter s)
  describe "digit" $ do
    it "fails on empty input" $
      null (runParser digit "")
    it "succeeds when first character is a digit" $
      forAll (sublistOf "0123456789" `suchThat` notNull) $ \s ->
        runParser digit s == [(tail s, head s)]
    it "fails when first character is not a digit" $
      forAll (vectorOf 10 (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZ°^!§$%&/()=?")) $ \s ->
        null (runParser digit s)
  describe "string" $ do
    it "fails on empty input" $
      null (runParser (string "abc") "")
    it "succeeds" $
      forAll randomInput $ \i ->
        forAll (choose (1, length i)) $ \x ->
          let prefix = take x i
           in runParser (string prefix) i == [(drop x i, prefix)]
  describe "tokens" $ do
    it "succeeds" $
      forAll (randomInput `suchThat` ((/= ' ') . head)) $ \i ->
        forAll (choose (1, length i)) $ \x ->
          forAll (choose (1, 100)) $ \n ->
            let prefix = take x i
                i' = replicate n ' ' ++ i
             in runParser (tokens prefix) i' == [(drop (length prefix) i, prefix)]
  describe "notOf" $ do
    it "fails on empty input" $
      null (runParser (notOf "abc") "")
    it "succeeds" $
      forAll (randomInput' 3) $ \n ->
        forAll (randomInput `suchThat` \xs -> head xs `notElem` n) $ \i ->
          runParser (notOf n) i == [(tail i, head i)]
    it "fails when first char is not allowed" $
      forAll (randomInput' 3) $ \n ->
          forAll (randomInput `suchThat` \xs -> head xs `elem` n) $ \i ->
            null (runParser (notOf n) i)
  describe "space" $ do
    it "fails on empty input" $
      null (runParser space "")
    it "succeeds when first character is a space" $
      forAll (randomInput `suchThat` ((' ' ==) . head)) $ \s ->
        runParser space s == [(tail s, ' ')]
    it "fails when first character is not a space" $
      forAll (randomInput `suchThat` ((' ' /=) . head)) $ \s ->
        null (runParser space s)
  describe "spaces" $ do
    it "succeeds when first character(s) is a space" $
      forAll (randomInput `suchThat` ((' ' /=) . head)) $ \s ->
        forAll (choose (2, 100)) $ \x ->
          let s' = replicate x ' ' ++ s
           in runParser spaces s' == [(drop a s', take a s') | a <- [x, (x - 1) .. 0]]
    it "succeeds when first character is not a space" $
      forAll (randomInput `suchThat` ((' ' /=) . head)) $ \s ->
        runParser spaces s == [(s, "")]
  describe "token" $ do
    it "fails on empty input" $
      null (runParser (token 'x') "")
    it "succeeds without leading spaces" $
      forAll (randomInput `suchThat` all isLetter) $ \s ->
        runParser (token (head s)) s == [(tail s, head s)]
    it "succeeds with leading spaces" $
      forAll (randomInput `suchThat` \v -> all isLetter v && not (null v)) $ \s ->
        forAll (choose (2, 100)) $ \x ->
          let s' = replicate x ' ' ++ s
           in runParser (token (head s)) s' == [(tail s, head s)]
  describe "between" $ do
    xit "tests" True
  describe "nat" $ do
    it "fails on empty input" $
      null (runParser nat "")
    it "parses the correct nat when input starts with digit(s)" $
      forAll (randomInput `suchThat` (not . isDigit . head)) $ \s ->
        forAll (choose (2, 10000 :: Int)) $ \x ->
          let s' = show x ++ s
              l = length $ show x
           in runParser nat s' == [(drop a s', read (take a s')) | a <- [l, l - 1 .. 1]]
  describe "numList" $ do
    it "fails on empty input" $
      null (runParser nat "")
    it "parses the empty list" $
      runParser numList "[]" == [("", [])]
    it "parses the correct list" $
      forAll (choose (1, 100 :: Int)) $ \listLength ->
        forAll (vectorOf listLength (choose (0, 100 :: Int))) $ \list ->
          runParser numList (show list) == [("", list)]
    it "parses the correct list with unnecessary spaces" $
      forAll (choose (1, 100 :: Int)) $ \listLength ->
        forAll (vectorOf listLength (choose (0, 100 :: Int))) $ \list ->
          let list' = replace "," " , " $ show list
           in runParser numList list' == [("", list)]
