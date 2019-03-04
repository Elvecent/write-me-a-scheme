module Expressions where

data SExp = Var String
          | Lambda [String] SExp
          | App [SExp]
          | Quote SExp
          deriving Show
