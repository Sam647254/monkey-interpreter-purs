module ParserTest where

import Prelude

import AST (Statement(..))
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Lexer.Lexer (createLexer)
import Parser (runParser)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Console (log)
import Token.Token (literal)

parserTests :: TestSuite
parserTests = suite "Parser tests" do
   test "let statements" do
      let
         input = "\
\let x = 5;\
\let y = 10;\
\let foobar = 838383;"
         expected = ["x", "y", "foobar"]
         result = runParser $ createLexer input

         checkStatement :: (Tuple Statement String) -> Aff Unit
         checkStatement (Tuple (LetStatement token id _) expected) = do
            equal "let" $ literal token
            equal expected id.value
      case result of
         Right program -> do
            let statements = program.statements
            _ <- traverse checkStatement (zip statements expected)
            pure unit
         Left errors -> do
            log $ show errors
            assert "Test failed" false