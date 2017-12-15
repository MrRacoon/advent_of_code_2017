module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["==","!=","<",">","<=",">=", "inc", "dec", "if"]
    names = []
    style = emptyDef
      { Tok.commentLine = "#"
      , Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      }

integer :: Parser Integer
integer = Tok.integer lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
