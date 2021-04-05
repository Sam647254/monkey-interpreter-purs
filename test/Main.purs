module Test.Main where

import Prelude

import Effect (Effect)
import ParserTest (parserTests)
import Test.LexerTest (lexerTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   lexerTests
   parserTests