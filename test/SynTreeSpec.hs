module SynTreeSpec where

import Parsing (Parser (runParser))
import SynTree (SynTree (..), display, parsePrettyTree, parseTree, pretty)
import Test.Hspec
import Test.QuickCheck (elements, forAll)

spec :: Spec
spec = do
  describe "display" $ do
    it "should display a Leaf correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        display (Leaf x) == [x]
    it "should display Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        display (Not (Leaf x)) == "~(" ++ [x] ++ ")"
    it "should display double Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        display (Not (Not (Leaf x))) == "~(" ++ "~(" ++ [x] ++ ")" ++ ")"
    it "should display a Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            display (Binary op (Leaf x) (Leaf y)) == "(" ++ [x] ++ ") " ++ [op] ++ " (" ++ [y] ++ ")"

  describe "pretty" $ do
    it "should display a Leaf correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        pretty (Leaf x) == [x]
    it "should display Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        pretty (Not (Leaf x)) == "~" ++ [x]
    it "should display double Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        pretty (Not (Not (Leaf x))) == "~~" ++ [x]
    it "should display Not Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            pretty (Not (Binary op (Leaf x) (Leaf y))) == "~(" ++ [x, ' ', op, ' ', y, ')']
    it "should display a Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            pretty (Binary op (Leaf x) (Leaf y)) == [x, ' ', op, ' ', y]
    it "should display a Double Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            pretty (Binary op (Leaf x) (Binary op (Leaf y) (Leaf x))) == [x, ' ', op] ++ " (" ++ [y, ' ', op, ' ', x, ')']

  describe "parseTree" $ do
    it "parse a Leaf correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Leaf x
         in runParser parseTree (display tree) == [("", tree)]
    it "parse Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Not $ Leaf x
         in runParser parseTree (display tree) == [("", tree)]
    it "parse double Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Not $ Not $ Leaf x
         in runParser parseTree (display tree) == [("", tree)]
    it "parse a Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            let tree = Binary op (Leaf x) (Leaf y)
             in runParser parseTree (display tree) == [("", tree)]

  describe "parsePrettyTree" $ do
    it "should parse a Leaf correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Leaf x
         in runParser parsePrettyTree (pretty tree) == [("", tree)]
    it "should parse Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Not $ Leaf x
         in runParser parsePrettyTree (pretty tree) == [("", tree)]
    it "should parse double Not correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        let tree = Not $ Not $ Leaf x
         in runParser parsePrettyTree (pretty tree) == [("", tree)]
    it "should parse Not Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            let tree = Not $ Binary op (Leaf x) (Leaf y)
             in runParser parsePrettyTree (pretty tree) == [("", tree)]
    it "should parse a Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            let tree = Binary op (Leaf x) (Leaf y)
             in runParser parsePrettyTree (pretty tree) == [("", tree)]
    it "should parse a Double Binary correctly" $
      forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \x ->
        forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \y ->
          forAll (elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") $ \op ->
            let tree = Binary op (Leaf x) $ Binary op (Leaf y) (Leaf x)
             in runParser parsePrettyTree (pretty tree) == [("", tree)]
