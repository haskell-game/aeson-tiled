module Codec.Tiled.Tileset.WangColor where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Property (Property)

data WangColor = WangColor
  { color       :: Text             -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB)
  , name        :: Text             -- ^ Name of Wang color
  , probability :: Double           -- ^ Probability used when randomizing
  , properties  :: Vector Property  -- ^ Array of Properties
  , tile        :: Int              -- ^ Local ID of tile representing terrain
  }
  deriving (Eq, Show, Generic)

instance FromJSON WangColor where
  parseJSON = genericParseJSON

instance ToJSON WangColor where
  toJSON = genericToJSON
