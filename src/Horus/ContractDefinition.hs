{-# LANGUAGE OverloadedStrings #-}

module Horus.ContractDefinition (ContractDefinition (..)) where

import Data.Aeson (FromJSON (..), withObject, withText, (.:))
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Map (Map)
import Data.SBV (SBool)
import Data.Text (Text, unpack)

import Horus.Program (Program)
import Horus.SMT2.Lexer (alexScanTokens)
import Horus.SMT2.Parser (parseAssertion)

data ContractDefinition = ContractDefinition
  { cd_program :: Program
  , cd_checks :: Checks
  }
  deriving (Show)

data Checks = Checks
  { c_preConds :: Map Text SBool
  , c_postConds :: Map Text SBool
  , c_invariants :: Map Text SBool
  }
  deriving (Show)

instance FromJSON ContractDefinition where
  parseJSON = withObject "ContractDefinition" $ \v ->
    ContractDefinition
      <$> v .: "program"
      <*> v .: "checks"

newtype HBool = HBool SBool

instance FromJSON HBool where
  parseJSON = withText "HBool" $ \t ->
    unpack t & alexScanTokens & parseAssertion & HBool & pure

elimHBool :: Map Text HBool -> Map Text SBool
elimHBool = coerce

instance FromJSON Checks where
  parseJSON = withObject "Checks" $ \v ->
    Checks
      <$> fmap elimHBool (v .: "pre_conds")
      <*> fmap elimHBool (v .: "post_conds")
      <*> fmap elimHBool (v .: "invariants")
