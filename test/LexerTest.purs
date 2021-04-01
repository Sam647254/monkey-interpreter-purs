module Test.LexerTest where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, lift, put, runState)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Lexer.Lexer (Lexer, createLexer, getNextToken)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Token.Token (Token(..), TokenType(..))

lexerTests :: TestSuite
lexerTests = suite "Lexer tests" do
   test "tokens" do
      let
         input = "=+(){},;"
         expected =
            [ Assignment
            , Plus
            , LParen
            , RParen
            , LBrace
            , RBrace
            , Comma
            , Semicolon
            , EOF
            ]
         
         expectTokens :: Array TokenType -> StateT Lexer Aff Unit
         expectTokens =
            traverse_
               \tokenType -> do
                  current <- get
                  let (Tuple (Token nextType _) next) = runState getNextToken current
                  put next
                  lift $ equal tokenType nextType
      evalStateT (expectTokens expected) (createLexer input)