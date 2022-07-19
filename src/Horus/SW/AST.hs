module Horus.SW.AST
  ( Expr (..)
  , NewOpData (..)
  , CairoType (..)
  )
where

import Data.List (intercalate)
import Data.Text (Text, unpack)

import Horus.SW.Instruction (PointerRegister (..))
import Horus.SW.ScopedName (ScopedName)

data CairoType
  = TypeFelt
  | TypeCodeoffset
  | TypePointer CairoType
  | TypeTuple [(Maybe ScopedName, Maybe CairoType)]
  | TypeStruct ScopedName
  deriving (Eq)

data NewOpData = NewOpData {nod_expr :: Expr, nod_isTyped :: Bool}
  deriving (Eq)

data Expr
  = Reg PointerRegister
  | Const Integer
  | Identifier ScopedName
  | Deref Expr
  | Subscript Expr Expr
  | Dot Expr Text
  | Cast Expr CairoType
  | ParenOrTuple [Expr]
  | Sum Expr Expr
  | Sub Expr Expr
  | Prod Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Neg Expr
  | AddrOf Expr
  | New NewOpData
  deriving (Eq)

instance Show CairoType where
  show TypeFelt = "felt"
  show TypeCodeoffset = "codeoffset"
  show (TypePointer pointee) = show pointee <> "*"
  show (TypeTuple mems) = "(" <> intercalate ", " (map (uncurry showTupleMember) mems) <> ")"
   where
    showTupleMember Nothing Nothing = ""
    showTupleMember (Just name) Nothing = show name
    showTupleMember Nothing (Just ty) = show ty
    showTupleMember (Just name) (Just ty) = show name <> ": " <> show ty
  show (TypeStruct name) = show name

instance Show Expr where
  show (Reg AllocationPointer) = "ap"
  show (Reg FramePointer) = "fp"
  show (Const int) = show int
  show (Identifier name) = show name
  show (Deref expr) = "[" <> show expr <> "]"
  show (Subscript atom expr) = show atom <> "[" <> show expr <> "]"
  show (Dot atom member) = show atom <> "." <> unpack member
  show (Cast expr cairoType) = "cast(" <> show expr <> ", " <> show cairoType <> ")"
  show (ParenOrTuple expr) = "(" <> show expr <> ")" -- TODO: rewrite this
  show (Sum a b) = "(" <> show a <> " + " <> show b <> ")"
  show (Sub a b) = "(" <> show a <> " - " <> show b <> ")"
  show (Prod a b) = "(" <> show a <> " * " <> show b <> ")"
  show (Div a b) = "(" <> show a <> " / " <> show b <> ")"
  show (Pow a b) = "(" <> show a <> " ** " <> show b <> ")"
  show (Neg a) = "(-" <> show a <> ")"
  show (AddrOf a) = "&" <> show a
  show (New (NewOpData a _isTyped)) = "new " <> show a
