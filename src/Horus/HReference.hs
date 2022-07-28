{-# OPTIONS_GHC -w #-}

module Horus.HReference (HReference (..), evalReference) where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Map (toList, (!), (!?))
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Data.Traversable (for)
import GHC.Num.Integer (integerFromInt)

import Horus.Label (Label, unLabel)
import Horus.Preprocessor (Model (..))
import Horus.Program (Identifiers)
import Horus.SW.AST (CairoType (..))
import Horus.SW.Identifier (Identifier (..), Member (..), Struct (..))
import Horus.SW.ScopedName (ScopedName (..))
import Horus.Util (maybeToError, tShow)
import SimpleSMT.Typed (TSExpr)

data HReference = HReference
  { href_name :: Text
  , href_type :: CairoType
  , href_expr :: TSExpr Integer
  , href_pc :: Label
  }

evalReference :: MonadError Text m => Model -> Identifiers -> HReference -> m Text
evalReference Model{..} identifiers HReference{..} =
  let name = href_name <> "!" <> tShow (unLabel href_pc)
   in case href_type of
        TypeFelt -> tShow <$> maybeToError ("Can't find " <> name) (m_refs !? name)
        TypeCodeoffset -> tShow <$> maybeToError ("Can't find " <> name) (m_refs !? name)
        TypePointer _ -> tShow <$> maybeToError ("Can't find " <> name) (m_refs !? name)
        TypeStruct structName -> do
          structAddr <- maybeToError ("Can't find " <> name) $ m_refs !? name
          -- throwError $ tShow m_refs
          evalStruct structAddr structName
        TypeTuple _ -> undefined
 where
  evalStruct :: MonadError Text m => Integer -> ScopedName -> m Text
  evalStruct structAddr structName = case identifiers !? structName of
    Just (IStruct Struct{..}) -> do
      members <- for (toList st_members) $ \(memberName, Member{..}) -> do
        let memAddr = structAddr + (integerFromInt me_offset)
        innerValue <- case me_cairoType of
          TypeFelt -> tShow <$> maybeToError ("Can't find " <> tShow memAddr) (m_mem !? memAddr)
          TypeCodeoffset -> tShow <$> maybeToError ("Can't find " <> tShow memAddr) (m_mem !? memAddr)
          TypePointer _ -> tShow <$> maybeToError ("Can't find " <> tShow memAddr) (m_mem !? memAddr)
          TypeStruct name -> evalStruct memAddr name
          TypeTuple _ -> undefined
        pure $ memberName <> " = " <> innerValue
      pure $ tShow structName <> "(" <> Text.intercalate ", " members <> ")"
    _ -> throwError ("Incorrect struct type: " <> tShow structName)
