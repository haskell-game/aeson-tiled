module Codec.Tiled.Tileset.WangTile where

import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data WangTile = WangTile
  { tileid :: Int        -- ^ Local ID of tile
  , wangid :: Vector Int -- ^ Array of Wang color indexes (uchar[8])
  }
  deriving (Eq, Show, Generic)

instance FromJSON WangTile where
  parseJSON = genericParseJSON

instance ToJSON WangTile where
  toJSON = genericToJSON
