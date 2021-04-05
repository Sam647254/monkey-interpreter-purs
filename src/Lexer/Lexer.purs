module Lexer.Lexer where

import Prelude

import Common (Errors, Log)
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Loops (whileM_)
import Control.Monad.State (State, StateT, execState, get, modify, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (many, slice)
import Data.Either (Either)
import Data.List (List, fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Token.Token (Token(..), TokenType(..))

type Lexer =
   { input :: String
   , position :: Int
   , readPosition :: Int
   , current :: Char
   }

type LexerState = StateT Lexer

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

readChar :: forall a. Monad a => LexerState a Lexer
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

isNotEOF' :: State (Tuple Lexer Token) Boolean
isNotEOF' = do
   (Tuple _ (Token tokenType _)) <- get
   pure $ tokenType /= EOF

isLetter :: Char -> Boolean
isLetter ch = 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'

isDigit :: Char -> Boolean
isDigit ch = '0' <= ch && ch <= '9'

checkCharType :: forall a. Monad a => (Char -> Boolean) -> StateT Lexer a Boolean
checkCharType predicate = do
   lexer <- get
   pure $ predicate lexer.current

isDigit' :: forall a. Monad a => StateT Lexer a Boolean
isDigit' = checkCharType isDigit

isWhitespace' :: forall a. Monad a => StateT Lexer a Boolean
isWhitespace' =
   checkCharType \s ->
      case s of
      ' ' -> true
      '\t' -> true
      '\n' -> true
      '\r' -> true
      _ -> false

isLetter' :: forall a. Monad a => StateT Lexer a Boolean
isLetter' = checkCharType isLetter

lookupIdentifier :: String -> TokenType
lookupIdentifier ident =
   case ident of
     "fn" -> Function
     "let" -> Let
     "true" -> True
     "false" -> False
     "if" -> If
     "else" -> Else
     "return" -> Return
     _ -> Identifier

readIdentifier :: forall a. Monad a => StateT Lexer a String
readIdentifier = do
   lexer <- get
   let start = lexer.position
   whileM_ isLetter' readChar
   lexer' <- get
   pure $ fromCharArray $ slice start lexer'.position $ toCharArray lexer.input

readNumber :: forall a. Monad a => StateT Lexer a String
readNumber = do
   lexer <- get
   let start = lexer.position
   whileM_ isDigit' readChar
   lexer' <- get
   pure $ fromCharArray $ slice start lexer'.position $ toCharArray lexer.input

getNextToken :: StateT Lexer (WriterT Log (Except Errors)) Token
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
            '<' -> LT
            '>' -> GT
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
   if tokenType == Illegal then
      throwError $ ["Unexpected input: " <> ch]
   else if tokenType == EOF then
      throwError ["EOF reached"]
   else do
      let token = Token tokenType ch
      pure token

runLexer :: forall output.
   StateT Lexer (WriterT Log (Except Errors)) output -> Lexer ->
      Either Errors (Tuple (Tuple output Lexer) Log)
runLexer action lexer = unwrap $ runExceptT $ runWriterT $ runStateT action lexer

getAllTokens :: Lexer -> Either Errors (List Token)
getAllTokens lexer = do
   (Tuple (Tuple tokens _) _) <- runLexer (many getNextToken) lexer
   pure $ fromFoldable tokens