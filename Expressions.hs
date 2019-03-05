module Expressions where

data SExp = Var String
          | Lambda [String] SExp
          | App [SExp]
          | Quote SExp
          | NumLit Int
          | StrLit String
          | Define String SExp
          deriving Show
