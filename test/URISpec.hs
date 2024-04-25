module URISpec where

import Test.Hspec
import Parsing (Parser(runParser), just)
import URI (parseURI, prettyURI, URI (..))
import Data.Map (empty, fromList)

standardURI :: URI
standardURI = URI {
  scheme = "https"
, credentials = Nothing
, domain = "uni-due.de"
, port = Nothing
, directory = "/fmi"
, params = empty
, hash = Nothing
}

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses standard uri" $
      runParser (just parseURI) (prettyURI standardURI) == [("", standardURI)]
    it "parses uri with port" $
      let withPort = standardURI { port = Just 3000 } in
      runParser (just parseURI) (prettyURI withPort) == [("", withPort)]
    it "parses uri with credentials" $
      let withCredentials = standardURI { credentials = Just ("root", "123456") } in
      runParser (just parseURI) (prettyURI withCredentials) == [("", withCredentials)]
    it "parses uri with params" $
      let withParams = standardURI { params = fromList [("a","b"),("c","d")] } in
      runParser (just parseURI) (prettyURI withParams) == [("", withParams)]
    it "parses uri with hash" $
      let withHash = standardURI { hash = Just "abcdef" } in
      runParser (just parseURI) (prettyURI withHash) == [("", withHash)]
    it "parses full uri" $
      let full = standardURI {
         port = Just 3000
       , credentials = Just ("root", "123456")
       , params = fromList [("a","b"),("c","d")]
       , hash = Just "abcdef" } in
      runParser (just parseURI) (prettyURI full) == [("", full)]
