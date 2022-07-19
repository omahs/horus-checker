{-# OPTIONS_GHC -fno-warn-orphans #-}

module Horus.SW.AST.JSON () where

import Data.Aeson (FromJSON (..), withText)
import Data.Text (unpack)

import Horus.SW.AST (CairoType, Expr)
import Horus.SW.AST.Lexer (alexScanTokens)
import Horus.SW.AST.Parser (parseCairoExpr, parseCairoType)

instance FromJSON CairoType where
  parseJSON = withText "CairoType" $ pure . parseCairoType . alexScanTokens . unpack

instance FromJSON Expr where
  parseJSON = withText "Expr" $ pure . parseCairoExpr . alexScanTokens . unpack
