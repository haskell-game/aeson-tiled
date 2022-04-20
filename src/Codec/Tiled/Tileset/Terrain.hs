module Codec.Tiled.Tileset.Terrain where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Property (Property)

data Terrain = Terrain
  { name       :: Text            -- ^ Name of terrain
  , properties :: Vector Property -- ^ array Array of Properties
  , tile       :: Int             -- ^ Local ID of tile representing terrain
  }
  deriving (Eq, Show, Generic)

instance FromJSON Terrain where
  parseJSON = genericParseJSON

instance ToJSON Terrain where
  toJSON = genericToJSON
