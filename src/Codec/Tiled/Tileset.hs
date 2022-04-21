module Codec.Tiled.Tileset where

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
  { backgroundcolor  :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
  , columns          :: Int                     -- ^ The number of tile columns in the tileset
  , firstgid         :: GID                     -- ^ GID corresponding to the first tile in the set
  , grid             :: Maybe Grid              -- ^ (optional)
  , image            :: FilePath                -- ^ Image used for tiles in this set
  , imageheight      :: Int                     -- ^ Height of source image in pixels
  , imagewidth       :: Int                     -- ^ Width of source image in pixels
  , margin           :: Int                     -- ^ Buffer between image edge and first tile (pixels)
  , name             :: Text                    -- ^ Name given to this tileset
  , objectalignment  :: Maybe Text              -- ^ Alignment to use for tile objects (unspecified (default), @topleft@, @top@, @topright@, @left@, @center@, @right@, @bottomleft@, @bottom@ or @bottomright@)
  , properties       :: Maybe (Vector Property) -- ^ Array of Properties
  , source           :: Maybe FilePath          -- ^ The external file that contains this tilesets data
  , spacing          :: Int                     -- ^ Spacing between adjacent tiles in image (pixels)
  , terrains         :: Maybe (Vector Terrain)  -- ^ Array of Terrains (optional)
  , tilecount        :: Int                     -- ^ The number of tiles in this tileset
  , tiledversion     :: Maybe Text              -- ^ The Tiled version used to save the file
  , tileheight       :: Int                     -- ^ Maximum height of tiles in this set
  , tileoffset       :: Maybe TileOffset        -- ^ (optional)
  , tiles            :: Maybe (Vector Tile)     -- ^ Array of Tiles (optional)
  , tilewidth        :: Int                     -- ^ Maximum width of tiles in this set
  , transformations  :: Maybe Transformations   -- ^ Allowed transformations (optional)
  , transparentcolor :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB) (optional)
  , type_            :: Maybe Text              -- ^ @tileset@ (for tileset files)
  , version          :: Maybe Text              -- ^ The JSON format version
  , wangsets         :: Maybe (Vector WangSet)  -- ^ Array of Wang sets
  }
  deriving (Eq, Show, Generic)

instance FromJSON Tileset where
  parseJSON = genericParseJSON

instance ToJSON Tileset where
  toJSON = genericToJSON
