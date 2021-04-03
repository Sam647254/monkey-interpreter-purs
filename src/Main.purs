module Main where

import Prelude

import Control.Monad.Loops (whileM)
import Control.Monad.State (State, evalState, get, put, runState)
import Data.Array (reverse)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Lexer.Lexer (Lexer, createLexer, getNextToken, isNotEOF')
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Token.Token (Token)

readNextToken :: State (Tuple Lexer Token) Token
readNextToken = do
   Tuple lexer _ <- get
   let (Tuple nextToken lexer') = runState getNextToken lexer
   put $ Tuple lexer' nextToken
   pure nextToken

main :: Effect Unit
main = do
   interface <- createConsoleInterface noCompletion
   setPrompt ">> " interface

   let
      lineHandler input = do
         let lexer = createLexer input
         let readTokens = whileM isNotEOF' readNextToken
         let (Tuple first lexer') = runState getNextToken lexer
         let tokens = evalState readTokens (Tuple lexer' first)
         traverse_ log $ map show $ reverse $ tokens <> [first]

         setLineHandler lineHandler interface
         prompt interface
         pure unit
   
   setLineHandler lineHandler interface
   prompt interface

   pure unit