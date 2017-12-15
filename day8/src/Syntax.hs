module Syntax where

type Name = String

data Op = If | Inc | Dec | EQ | NE | GT | GTE | LT | LTE
  deriving (Eq, Ord, Show)

data Expr
  = Register String
  | Value Int
  | BinOp Op Expr Expr
  deriving (Eq, Ord, Show)
