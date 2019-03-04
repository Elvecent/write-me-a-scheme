{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, unpack)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty(..))

data Sexp = Var String
          | Lambda [String] Sexp
          | App [Sexp]
          | Quote Sexp
          deriving Show

type Parser = Parsec Void Text

mySpace = L.space
  space1
  (L.skipLineComment ";")
  (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme mySpace

symbol :: Text -> Parser Text
symbol = L.symbol mySpace

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

flatList :: Parser [String]
flatList = parens . many . lexeme $ identifier

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

topLevel :: Parser [Sexp]
topLevel = many sparser <* eof

sparser :: Parser Sexp
sparser = label "s-expression" $
  choice [var, quote, try lambda, App <$> parens (some sparser)]

var :: Parser Sexp
var = label "variable" $ do
  id <- lexeme identifier
  when (id == "lambda") $
    fail "Don't abuse the poor lambda!"
  return $ Var id

lambda :: Parser Sexp
lambda = label "lambda" $ do
  space
  (args, body) <- parens $ do
    _ <- lexeme "lambda"
    args <- flatList
    body <- sparser
    return (args, body)
  return $ Lambda args body

quote :: Parser Sexp
quote = Quote <$> (char '\'' *> sparser)

main = return ()
