module Codec.Tiled.Tileset.TileOffset where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data TileOffset = TileOffset
  { x :: Int -- ^ Horizontal offset in pixels
  , y :: Int -- ^ Vertical offset in pixels (positive is down)
  }
  deriving (Eq, Show, Generic)

instance FromJSON TileOffset where
  parseJSON = genericParseJSON

instance ToJSON TileOffset where
  toJSON = genericToJSON
