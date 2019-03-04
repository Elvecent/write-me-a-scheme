{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad              (when)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Text                  (Text, unpack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data SExp = Var String
          | Lambda [String] SExp
          | App [SExp]
          | Quote SExp
          | NumLit Int
          | StrLit String
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

topLevel :: Parser [SExp]
topLevel = many sparser <* eof

sparser :: Parser SExp
sparser = label "s-expression" $
  choice [literal, var, quote, try lambda, App <$> parens (some sparser)]

literal :: Parser SExp
literal = choice $ map (<* space) [strLit, numLit]
  where
    numLit = NumLit <$> L.signed (return ()) L.decimal
    strLit = StrLit <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

var :: Parser SExp
var = label "variable" $ do
  id <- lexeme identifier
  when (id == "lambda") $
    fail "Don't abuse the poor lambda!"
  return $ Var id

lambda :: Parser SExp
lambda = label "lambda" $ do
  space
  (args, body) <- parens $ do
    _ <- lexeme "lambda"
    args <- flatList
    body <- sparser
    return (args, body)
  return $ Lambda args body

quote :: Parser SExp
quote = Quote <$> (char '\'' *> sparser)
