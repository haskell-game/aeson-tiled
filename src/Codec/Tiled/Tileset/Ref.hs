module Codec.Tiled.Tileset.Ref where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset.Grid (Grid)
import Codec.Tiled.Tileset.Terrain (Terrain)
import Codec.Tiled.Tileset.Tile (Tile)
import Codec.Tiled.Tileset.TileOffset (TileOffset)
import Codec.Tiled.Tileset.Transformations (Transformations)
import Codec.Tiled.Tileset.WangSet (WangSet)
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
