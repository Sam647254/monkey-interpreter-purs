module Token.Token where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data TokenType
   = Illegal
   | EOF
   | Identifier
   | Integer
   | Assignment
   | Plus
   | Comma
   | Semicolon
   | LParen
   | RParen
   | LBrace
   | RBrace
   | Function
   | Let

data Token = Token TokenType String

derive instance genericTokenType :: Generic TokenType _

derive instance eqToken :: Eq TokenType

instance showTokenType :: Show TokenType where
   show = genericShow