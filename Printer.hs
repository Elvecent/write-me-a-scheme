{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Expressions

instance Pretty SExp where
  pretty = \case
    Var str -> pretty str
    Lambda vs s -> align . parens . nest 2 . vsep $
      [ sep ["lambda", parens . sep . fmap pretty $ vs]
      , pretty s
      ]
    App ss -> parens . fillSep . fmap pretty $ ss
    Quote s -> fillCat ["\'", pretty s]
    NumLit n -> pretty n
    StrLit s -> cat ["\"", pretty s, "\""]

ppSExp :: SExp -> String
ppSExp = pretty >>>
  layoutPretty defaultLayoutOptions >>>
  renderString
  where
    (>>>) = flip (.)
