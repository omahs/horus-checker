module Horus.SW.ApTracking (ApTracking (..)) where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (FromJSON (..))

data ApTracking = ApTracking {at_group :: Int, at_offset :: Int}
  deriving stock (Show)

instance FromJSON ApTracking where
  parseJSON = withObject "ap_tracking" $ \v ->
    ApTracking <$> v .: "group" <*> v .: "offset"
