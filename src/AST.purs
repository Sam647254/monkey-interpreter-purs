module AST where

import Token.Token (Token, literal)

class Node a where
   tokenLiteral :: a -> String

type Program = { statements :: Array Statement }

type Identifier =
   { token :: Token
   , value :: String
   }

data Expression
   = Identifier Identifier
   | Dummy

data Statement
   = LetStatement Token Identifier Expression
   | ReturnStatement Token Expression

instance nodeExpression :: Node Expression where
   tokenLiteral (Identifier id) = literal id.token
   tokenLiteral Dummy = ""

instance nodeStatement :: Node Statement where
   tokenLiteral (LetStatement token _ _) = literal token

   tokenLiteral (ReturnStatement token _) = literal token