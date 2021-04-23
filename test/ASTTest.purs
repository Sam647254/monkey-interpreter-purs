module Test.ASTTest where

import Prelude

import AST as A
import AST (Expression(..), Program(..), Statement(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Token.Token (Token(..), TokenType(..))
import Token.Token as T

testString :: TestSuite
testString = suite "String tests" do
   test "program 1" do
      let
         program =
            Program
               { statements:
                  [ LetStatement
                     (Token Let "let")
                     (A.Identifier
                        { token: (Token T.Identifier "myVar")
                        , value: "myVar"
                        })
                     (Id
                        (A.Identifier
                           { token: (Token T.Identifier "anotherVar")
                           , value: "anotherVar"
                           }))
                  ]
               }
      
      equal "let myVar = anotherVar;" (show program)