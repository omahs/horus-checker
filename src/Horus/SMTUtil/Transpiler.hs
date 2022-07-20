module Horus.SMTUtil.Transpiler (exprToSMT) where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Text (Text)

import Horus.SMTUtil (memory, regToTSExpr)
import Horus.SW.AST (CairoType (..), Expr (..))
import Horus.Util (tShow)
import SimpleSMT.Typed (TSExpr)

exprToSMT :: MonadError Text m => Expr -> m (TSExpr Integer)
exprToSMT (Reg reg) = pure $ regToTSExpr reg
exprToSMT (Const int) = pure $ fromInteger int
exprToSMT (Identifier name) = throwError ("Unexpected identifier: " <> tShow name)
exprToSMT (Deref expr) = memory <$> exprToSMT expr
exprToSMT (Subscript expr sub) = do
  expr' <- exprToSMT expr
  sub' <- exprToSMT sub
  pure $ memory (expr' + sub')
exprToSMT expr@(Dot _ _) =
  throwError
    ( "Unexpected expression: "
        <> tShow expr
    )
exprToSMT (Cast expr ty) = case ty of
  TypeFelt -> exprToSMT expr
  TypeCodeoffset -> exprToSMT expr
  TypePointer _ -> exprToSMT expr
  _ -> throwError ("Cannot turn into SMT an expression of type " <> tShow ty)
exprToSMT expr@(ParenOrTuple []) = throwError ("Cannot tranform " <> tShow expr)
exprToSMT (ParenOrTuple (expr : _)) = exprToSMT expr
exprToSMT (Sum a b) = do
  a' <- exprToSMT a
  b' <- exprToSMT b
  pure $ a' + b'
exprToSMT (Sub a b) = do
  a' <- exprToSMT a
  b' <- exprToSMT b
  pure $ a' - b'
exprToSMT (Prod a b) = do
  a' <- exprToSMT a
  b' <- exprToSMT b
  pure $ a' * b'
exprToSMT (Div _ _) = throwError "Division is not supported yet."
exprToSMT (Pow _ _) = throwError "Power operator is not supported yet."
exprToSMT (Neg a) = do
  a' <- exprToSMT a
  pure $ -a'
exprToSMT (AddrOf _) = throwError "& operator is not supported yet."
exprToSMT (New _) = throwError "new operator is not supported yet."
