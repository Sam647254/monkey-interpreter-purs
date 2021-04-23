module AST where

import Data.Foldable (foldl)
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Token.Token (Token, literal)

class Show a <= Node a where
   tokenLiteral :: a -> String

newtype Program = Program { statements :: Array Statement }

newtype Identifier =
   Identifier
      { token :: Token
      , value :: String
      }

data Expression
   = Id Identifier
   | Dummy

data Statement
   = LetStatement Token Identifier Expression
   | ReturnStatement Token Expression
   | ExpressionStatement Token Expression

instance showExpression :: Show Expression where
   show _ = ""

instance nodeExpression :: Node Expression where
   tokenLiteral (Id (Identifier id)) = literal id.token
   tokenLiteral Dummy = ""

instance showStatement :: Show Statement where
   show (LetStatement token id value) =
      (literal token) <> " " <> (show id) <> " = " <> (show value) <> ";"
   
   show (ReturnStatement token value) =
      (literal token) <> " " <> (show value)
   
   show (ExpressionStatement _ expr) = show expr

instance showIdentifier :: Show Identifier where
   show (Identifier id) = literal id.token

instance nodeStatement :: Node Statement where
   tokenLiteral (LetStatement token _ _) = literal token

   tokenLiteral (ReturnStatement token _) = literal token

   tokenLiteral (ExpressionStatement token _) = literal token

instance nodeProgram :: Node Program where
   tokenLiteral program = ""

instance showProgram :: Show Program where
   show (Program program) = foldl (\str sek -> str <> (show sek)) "" program.statements