module Lexer.Lexer where

import Prelude

import Control.Monad.State (State, execState, get, modify, put)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (charAt)
import Token.Token (Token(..), TokenType(..))

type Lexer =
   { input :: String
   , position :: Int
   , readPosition :: Int
   , current :: Char
   }

createLexer :: String -> Lexer
createLexer input =
   let
      lexer =
         { input: input
         , position: 0
         , readPosition: 0
         , current: '\x00' }
   in do
      execState readChar lexer

readChar :: State Lexer Lexer
readChar =
   modify
      \lexer ->
         let nextChar = fromMaybe '\x00' $ charAt lexer.readPosition lexer.input
         in
         lexer
            { position = lexer.readPosition
            , readPosition = lexer.readPosition + 1
            , current = nextChar
            }

getNextToken :: State Lexer Token
getNextToken = do
   lexer <- get
   
   let
      tokenType =
         case lexer.current of
            '=' -> Assignment
            ';' -> Semicolon
            '(' -> LParen
            ')' -> RParen
            ',' -> Comma
            '+' -> Plus
            '{' -> LBrace
            '}' -> RBrace
            '\x00' -> EOF
            _ -> Illegal
      ch = if lexer.current == '\x00' then "" else show lexer.current
      token = Token tokenType ch
   put (execState readChar lexer)
   pure token