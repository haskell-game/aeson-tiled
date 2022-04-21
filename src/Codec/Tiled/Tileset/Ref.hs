module Codec.Tiled.Tileset.Ref where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Tiled.GID (GID)

data TilesetRef = TilesetRef
  { firstgid         :: GID      -- ^ GID corresponding to the first tile in the set
  , source           :: FilePath -- ^ The external file that contains this tilesets data
  }
  deriving (Eq, Show, Generic)

instance FromJSON TilesetRef where
  parseJSON = genericParseJSON

instance ToJSON TilesetRef where
  toJSON = genericToJSON
