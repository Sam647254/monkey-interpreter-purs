module Main where

import Prelude

import Data.Array (many, reverse)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Lexer.Lexer (createLexer, getNextToken, runLexer)
import Node.ReadLine (createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

main :: Effect Unit
main = do
   interface <- createConsoleInterface noCompletion
   setPrompt ">> " interface

   let
      lineHandler :: String -> Effect Unit
      lineHandler input = do
         let lexer = createLexer input
         let result = runLexer (many getNextToken) lexer
         case result of
            Right (Tuple (Tuple tokens _) _) ->
               traverse_ log $ map show $ reverse $ tokens
            Left errors -> log $ show errors

         setLineHandler lineHandler interface
         prompt interface
         pure unit
   
   setLineHandler lineHandler interface
   prompt interface

   pure unit