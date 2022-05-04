module Codec.Tiled.Layer.Chunk where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Layer.Data (LayerData)

data Chunk = Chunk
  { data_  :: LayerData -- ^ Array of unsigned int (GIDs) or base64-encoded data
  , height :: Int       -- ^ Height in tiles
  , width  :: Int       -- ^ Width in tiles
  , x      :: Int       -- ^ X coordinate in tiles
  , y      :: Int       -- ^ Y coordinate in tiles
  }
  deriving (Eq, Show, Generic)

instance FromJSON Chunk where
  parseJSON = genericParseJSON

instance ToJSON Chunk where
  toJSON = genericToJSON
