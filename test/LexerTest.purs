module Test.LexerTest where

import Prelude

import Control.Monad.State (evalStateT, get, lift, put, runState)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Lexer.Lexer (createLexer, getNextToken)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Token.Token (Token(..), TokenType(..))

expectTokens :: Array (Tuple TokenType String) -> String -> Aff Unit
expectTokens expected input =
   let
      expectTokens' =
         traverse_
            \(Tuple tokenType token) -> do
               current <- get
               let (Tuple (Token nextType nextToken) next) = runState getNextToken current
               put next
               lift $ equal tokenType nextType
               lift $ equal token nextToken
   in
   evalStateT (expectTokens' expected) (createLexer input)

lexerTests :: TestSuite
lexerTests = suite "Lexer tests" do
   test "tokens" do
      let
         input = "=+(){},;"
         expected =
            [ Tuple Assignment "="
            , Tuple Plus "+"
            , Tuple LParen "("
            , Tuple RParen ")"
            , Tuple LBrace "{"
            , Tuple RBrace "}"
            , Tuple Comma ","
            , Tuple Semicolon ";"
            , Tuple EOF ""
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
            [ Tuple Let "let"
            , Tuple Identifier "five"
            , Tuple Assignment "="
            , Tuple Integer "5"
            , Tuple Semicolon ";"
            , Tuple Let "let"
            , Tuple Identifier "ten"
            , Tuple Assignment "="
            , Tuple Integer "10"
            , Tuple Semicolon ";"
            , Tuple Let "let"
            , Tuple Identifier "add"
            , Tuple Assignment "="
            , Tuple Function "fn"
            , Tuple LParen "("
            , Tuple Identifier "x"
            , Tuple Comma ","
            , Tuple Identifier "y"
            , Tuple RParen ")"
            , Tuple LBrace "{"
            , Tuple Identifier "x"
            , Tuple Plus "+"
            , Tuple Identifier "y"
            , Tuple Semicolon ";"
            , Tuple RBrace "}"
            , Tuple Semicolon ";"
            , Tuple If "if"
            , Tuple LParen "("
            , Tuple Integer "5"
            , Tuple LT "<"
            , Tuple Integer "10"
            , Tuple RParen ")"
            , Tuple LBrace "{"
            , Tuple Return "return"
            , Tuple True "true"
            , Tuple Semicolon ";"
            , Tuple RBrace "}"
            , Tuple Else "else"
            , Tuple LBrace "{"
            , Tuple Return "return"
            , Tuple False "false"
            , Tuple Semicolon ";"
            , Tuple RBrace "}"
            ]
      expectTokens expected input