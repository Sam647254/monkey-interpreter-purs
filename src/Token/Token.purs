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
   | Minus
   | Bang
   | Asterisk
   | Slash
   | LT
   | GT
   | Comma
   | Semicolon
   | LParen
   | RParen
   | LBrace
   | RBrace
   | Function
   | Let
   | True
   | False
   | If
   | Else
   | Return

data Token = Token TokenType String

derive instance genericTokenType :: Generic TokenType _

derive instance eqToken :: Eq TokenType

derive instance genericToken :: Generic Token _

instance showTokenType :: Show TokenType where
   show = genericShow

instance showToken :: Show Token where
   show = genericShow