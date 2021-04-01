module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.LexerTest (lexerTests)

main :: Effect Unit
main = runTest do
   lexerTests