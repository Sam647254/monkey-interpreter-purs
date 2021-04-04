module Test.LexerTest where

import Prelude


import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Lexer.Lexer (createLexer, getAllTokens)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (log)
import Token.Token (Token(..), TokenType(..))

expectTokens :: Array Token -> String -> Aff Unit
expectTokens expected input =
   let
      actualTokens = getAllTokens (createLexer input)
   in
   case actualTokens of
   Left errors -> log $ show errors
   Right (Tuple (Tuple tokens _) _) -> equal expected tokens

lexerTests :: TestSuite
lexerTests = suite "Lexer tests" do
   test "tokens" do
      let
         input = "=+(){},;"
         expected =
            [ Token Assignment "="
            , Token Plus "+"
            , Token LParen "("
            , Token RParen ")"
            , Token LBrace "{"
            , Token RBrace "}"
            , Token Comma ","
            , Token Semicolon ";"
            ]
      expectTokens expected input
   
   test "tokens 2" do
      let
         input = "\
\let five = 5;\
\let ten = 10;\
\\
\let add = fn(x, y) {\
\   x + y;\
\};\
\if (5 < 10) {\
\   return true;\
\} else {\
\   return false;\
\}"
         expected =
            [ Token Let "let"
            , Token Identifier "five"
            , Token Assignment "="
            , Token Integer "5"
            , Token Semicolon ";"
            , Token Let "let"
            , Token Identifier "ten"
            , Token Assignment "="
            , Token Integer "10"
            , Token Semicolon ";"
            , Token Let "let"
            , Token Identifier "add"
            , Token Assignment "="
            , Token Function "fn"
            , Token LParen "("
            , Token Identifier "x"
            , Token Comma ","
            , Token Identifier "y"
            , Token RParen ")"
            , Token LBrace "{"
            , Token Identifier "x"
            , Token Plus "+"
            , Token Identifier "y"
            , Token Semicolon ";"
            , Token RBrace "}"
            , Token Semicolon ";"
            , Token If "if"
            , Token LParen "("
            , Token Integer "5"
            , Token LT "<"
            , Token Integer "10"
            , Token RParen ")"
            , Token LBrace "{"
            , Token Return "return"
            , Token True "true"
            , Token Semicolon ";"
            , Token RBrace "}"
            , Token Else "else"
            , Token LBrace "{"
            , Token Return "return"
            , Token False "false"
            , Token Semicolon ";"
            , Token RBrace "}"
            ]
      expectTokens expected input