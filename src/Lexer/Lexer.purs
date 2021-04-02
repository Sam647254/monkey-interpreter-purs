module Lexer.Lexer where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Loops (whileM, whileM_)
import Control.Monad.State (State, execState, get, modify, put)
import Data.Array (slice)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
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

isLetter :: Char -> Boolean
isLetter ch = 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'

isDigit :: Char -> Boolean
isDigit ch = '0' <= ch && ch <= '9'

isDigit' :: State Lexer Boolean
isDigit' = do
   lexer <- get
   pure $ isDigit lexer.current

isWhitespace' :: State Lexer Boolean
isWhitespace' = do
   lexer <- get
   pure $ case lexer.current of
      ' ' -> true
      '\t' -> true
      '\n' -> true
      '\r' -> true
      _ -> false

isLetter' :: State Lexer Boolean
isLetter' = do
   lexer <- get
   pure $ isLetter lexer.current

lookupIdentifier :: String -> TokenType
lookupIdentifier ident =
   case ident of
     "fn" -> Function
     "let" -> Let
     _ -> Identifier

readIdentifier :: State Lexer String
readIdentifier = do
   lexer <- get
   let start = lexer.position
   whileM_ isLetter' readChar
   lexer' <- get
   pure $ fromCharArray $ slice start lexer'.position $ toCharArray lexer.input

readNumber :: State Lexer String
readNumber = do
   lexer <- get
   let start = lexer.position
   whileM_ isDigit' readChar
   lexer' <- get
   pure $ fromCharArray $ slice start lexer'.position $ toCharArray lexer.input

getNextToken :: State Lexer Token
getNextToken = do
   whileM_ isWhitespace' readChar
   lexer <- get
   ch <-
      if lexer.current == '\x00' then
         pure ""
      else if isLetter lexer.current then
         readIdentifier
      else if isDigit lexer.current then
         readNumber
      else do
         put (execState readChar lexer)
         pure $ fromCharArray [lexer.current]
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
            _ ->
               if isLetter lexer.current then
                  lookupIdentifier ch
               else if isDigit lexer.current then
                  Integer
               else
                  Illegal
   let token = Token tokenType ch
   pure token