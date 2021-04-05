module Parser where

import Prelude

import AST (Statement(..), Program)
import AST as A
import Common (Log, Errors)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Loops (whileM, whileM_)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (reverse)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Lexer.Lexer (Lexer, getAllTokens)
import Token.Token (Token(..), TokenType(..), literal)
import Token.Token (TokenType(..)) as T

type Parser =
   { tokens :: List Token
   , currentToken :: Token
   , peekToken :: Token
   }

type ParserState = StateT Parser (WriterT Log (Except Errors))

nextToken :: ParserState Unit
nextToken = do
   parser <- get
   let
      currentToken' = parser.peekToken
      peekToken' =
         case parser.tokens of
            (next : _) -> next
            empty -> Token EOF ""
      remainingTokens =
         case parser.tokens of
            (_ : rest) -> rest
            empty -> empty
      parser' =
         parser
            { tokens = remainingTokens
            , currentToken = currentToken'
            , peekToken = peekToken'
            }
   put parser'

runParser :: Lexer -> Either Errors Program
runParser lexer = do
   tokens <- getAllTokens lexer
   parser <- do
      case createParser tokens of
         Just p -> pure p
         Nothing -> Left ["Not enough tokens to create parser"]
   (Tuple program _) <- runExcept $ runWriterT $ evalStateT parseProgram parser
   pure program

createParser :: List Token -> Maybe Parser
createParser (first : second : rest) =
   Just { tokens: rest, currentToken: first, peekToken: second }
createParser _ = Nothing

parseProgram :: ParserState Program
parseProgram = do
   statements <- whileM (isNot EOF) parseStatement
   pure { statements: reverse statements }

parseStatement :: ParserState Statement
parseStatement = do
   parser <- get
   let (Token tokenType _) = parser.currentToken
   statement <-
      case tokenType of
         Let -> parseLetStatement
         t -> throwError ["Parsing " <> (show t) <> " not implemented"]
   nextToken
   pure statement

parseLetStatement :: ParserState Statement
parseLetStatement = do
   parser <- get
   let letToken = parser.currentToken
   expectPeek T.Identifier
   parser' <- get
   let id = { token: parser'.currentToken, value: literal parser'.currentToken }
   expectPeek Assignment
   whileM_ (isNot Semicolon) nextToken
   pure $ LetStatement letToken id (A.Identifier id)

expectPeek :: TokenType -> ParserState Unit
expectPeek tokenType = do
   parser <- get
   if peekTokenIs parser tokenType then do
      nextToken
      pure unit
   else
      throwError ["Expected " <> (show tokenType) <> ", saw " <> (show parser.currentToken)]

currentTokenIs :: Parser -> TokenType -> Boolean
currentTokenIs parser tokenType =
   let (Token actualType _) = parser.currentToken
   in
   actualType == tokenType

peekTokenIs :: Parser -> TokenType -> Boolean
peekTokenIs parser tokenType =
   let (Token actualType _) = parser.peekToken
   in
   actualType == tokenType

isNot :: forall a. Monad a => TokenType -> StateT Parser a Boolean
isNot tokenType = do
   parser <- get
   pure $ not $ currentTokenIs parser tokenType
