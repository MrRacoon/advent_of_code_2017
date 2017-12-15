module Parser where

import Prelude hiding (EQ, GT, LT)

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

table =
  [ [ binary "inc" Inc Ex.AssocLeft
    , binary "dec" Dec Ex.AssocLeft
    ]
  , [ binary "==" EQ Ex.AssocLeft
    , binary "!=" NE Ex.AssocLeft
    ]
  , [ binary "<" LT Ex.AssocLeft
    , binary "<=" LTE Ex.AssocLeft
    , binary ">" GT Ex.AssocLeft
    , binary ">=" GTE Ex.AssocLeft
    ]
  , [ binary "if" If Ex.AssocRight ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

int = do
  n <- integer
  return $ Value $ fromInteger n

reg = do
  n <- identifier
  return $ Register n

-- =============================================================================

factor :: Parser Expr
factor
   = try int
 <|> try reg

-- =============================================================================

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel =
  many expr

parseExpr :: String -> Either ParseError Expr
parseExpr =
  parse (contents expr) "<stdin>"

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel =
  parse (contents toplevel) "<stdin>"
