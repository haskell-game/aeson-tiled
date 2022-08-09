module Codec.Tiled.Tileset
  ( Tileset(..)
  , empty
  ) where

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

data Tileset = Tileset
  { backgroundColor  :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
  , columns          :: Int                     -- ^ The number of tile columns in the tileset
  , firstGid         :: Maybe GID               -- ^ GID corresponding to the first tile in the set
  , grid             :: Maybe Grid              -- ^ (optional)
  , image            :: FilePath                -- ^ Image used for tiles in this set
  , imageHeight      :: Int                     -- ^ Height of source image in pixels
  , imageWidth       :: Int                     -- ^ Width of source image in pixels
  , margin           :: Int                     -- ^ Buffer between image edge and first tile (pixels)
  , name             :: Text                    -- ^ Name given to this tileset
  , objectAlignment  :: Maybe Text              -- ^ Alignment to use for tile objects (unspecified (default), @topleft@, @top@, @topright@, @left@, @center@, @right@, @bottomleft@, @bottom@ or @bottomright@)
  , properties       :: Maybe (Vector Property) -- ^ Array of Properties
  , source           :: Maybe FilePath          -- ^ The external file that contains this tilesets data
  , spacing          :: Int                     -- ^ Spacing between adjacent tiles in image (pixels)
  , terrains         :: Maybe (Vector Terrain)  -- ^ Array of Terrains (optional)
  , tileCount        :: Int                     -- ^ The number of tiles in this tileset
  , tiledVersion     :: Maybe Text              -- ^ The Tiled version used to save the file
  , tileHeight       :: Int                     -- ^ Maximum height of tiles in this set
  , tileOffset       :: Maybe TileOffset        -- ^ (optional)
  , tiles            :: Maybe (Vector Tile)     -- ^ Array of Tiles (optional)
  , tileWidth        :: Int                     -- ^ Maximum width of tiles in this set
  , transformations  :: Maybe Transformations   -- ^ Allowed transformations (optional)
  , transparentColor :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB) (optional)
  , type_            :: Maybe Text              -- ^ @tileset@ (for tileset files)
  , version          :: Maybe Text              -- ^ The JSON format version
  , wangSets         :: Maybe (Vector WangSet)  -- ^ Array of Wang sets
  }
  deriving (Eq, Show, Generic)

instance FromJSON Tileset where
  parseJSON = genericParseJSON

instance ToJSON Tileset where
  toJSON = genericToJSON

empty :: Tileset
empty =  Tileset
  { backgroundColor  = Nothing
  , columns          = 0
  , firstGid         = Nothing
  , grid             = Nothing
  , image            = ""
  , imageHeight      = 0
  , imageWidth       = 0
  , margin           = 0
  , name             = ""
  , objectAlignment  = Nothing
  , properties       = Nothing
  , source           = Nothing
  , spacing          = 0
  , terrains         = Nothing
  , tileCount        = 0
  , tiledVersion     = Nothing
  , tileHeight       = 0
  , tileOffset       = Nothing
  , tiles            = Nothing
  , tileWidth        = 0
  , transformations  = Nothing
  , transparentColor = Nothing
  , type_            = Nothing
  , version          = Nothing
  , wangSets         = Nothing
  }
