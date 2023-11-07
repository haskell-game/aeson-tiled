module Codec.Tiled.Property where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Property = Property
  { name         :: Text       -- ^ Name of the property
  , type_        :: Maybe Text -- ^ Type of the property (@string@ (default), @int@, @float@, @bool@, @color@, @file@, @object@ or @class@)
  , propertyType :: Maybe Text -- ^ Name of the custom property type, when applicable
  , value        :: Value      -- ^ Value of the property
  }
  deriving (Eq, Show, Generic)

instance FromJSON Property where
  parseJSON = genericParseJSON

instance ToJSON Property where
  toJSON = genericToJSON
