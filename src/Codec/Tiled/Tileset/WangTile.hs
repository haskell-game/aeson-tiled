module Codec.Tiled.Tileset.WangTile where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset.WangColor (WangColor)

data WangTile = WangTile
  { tileid :: Int        -- ^ Local ID of tile
  , wangid :: Vector Int -- ^ Array of Wang color indexes (uchar[8])
  }
  deriving (Eq, Show, Generic)

instance FromJSON WangTile where
  parseJSON = genericParseJSON

instance ToJSON WangTile where
  toJSON = genericToJSON
