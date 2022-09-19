module Horus.Program
  ( Program (..)
  , DebugInfo (..)
  , ILInfo (..)
  , FlowTrackingData (..)
  , ApTracking (..)
  , Identifiers
  )
where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map (keys)
import Numeric (readHex)

import Horus.Label (Label (..))
import Horus.SW.Identifier (Identifier)
import Horus.SW.ScopedName (ScopedName)

type Identifiers = Map ScopedName Identifier

data Program = Program
  { p_attributes :: [String]
  , p_builtins :: [String]
  , p_code :: [Integer]
  , p_identifiers :: Identifiers
  , p_mainScope :: String
  , p_prime :: Integer
  , p_debugInfo :: DebugInfo
  }

data DebugInfo = DebugInfo
  {di_instructionLocations :: Map Label ILInfo}
  deriving (Show)

data ILInfo = ILInfo
  { il_accessibleScopes :: [ScopedName]
  , il_flowTrackingData :: FlowTrackingData
  }
  deriving (Show)

data FlowTrackingData = FlowTrackingData
  { ftd_apTracking :: ApTracking
  , ftd_references :: [ScopedName]
  }
  deriving stock (Show)

data ApTracking = ApTracking {at_group :: Int, at_offset :: Int}
  deriving stock (Show)

instance FromJSON Program where
  parseJSON = withObject "Program" $ \v ->
    Program
      <$> v .: "attributes"
      <*> v .: "builtins"
      <*> (v .: "data" >>= traverse parseHexInteger)
      <*> v .: "identifiers"
      <*> v .: "main_scope"
      <*> (v .: "prime" >>= parseHexInteger)
      <*> (v .: "debug_info")

instance FromJSON DebugInfo where
  parseJSON = withObject "debug_info" $ \v ->
    DebugInfo <$> (v .: "instruction_locations")

instance FromJSON ILInfo where
  parseJSON = withObject "ILInfo" $ \v ->
    ILInfo
      <$> v .: "accessible_scopes"
      <*> v .: "flow_tracking_data"

instance FromJSON FlowTrackingData where
  parseJSON = withObject "flow_tracking_data" $ \v ->
    FlowTrackingData
      <$> v .: "ap_tracking"
      <*> (v .: "reference_ids" <&> Map.keys @_ @Int)

instance FromJSON ApTracking where
  parseJSON = withObject "ap_tracking" $ \v ->
    ApTracking <$> v .: "group" <*> v .: "offset"

parseHexInteger :: String -> Parser Integer
parseHexInteger ('0' : 'x' : rest)
  | [(res, "")] <- readHex rest = pure res
parseHexInteger arg = fail ("Can't parse '" <> arg <> "' as hex")
