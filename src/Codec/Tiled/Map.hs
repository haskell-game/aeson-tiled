module Codec.Tiled.Map where

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Layer (Layer)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset.Ref (TilesetRef)

data Map = Map
  { backgroundColor  :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
  , compressionLevel :: Maybe Int               -- ^ The compression level to use for tile layer data (defaults to -1, which means to use the algorithm default)
  , height           :: Int                     -- ^ Number of tile rows
  , hexSideLength    :: Maybe Int               -- ^ Length of the side of a hex tile in pixels (hexagonal maps only)
  , infinite         :: Maybe Bool              -- ^ Whether the map has infinite dimensions
  , layers           :: Vector Layer            -- ^ Array of Layers
  , nextLayerId      :: Int                     -- ^ Auto-increments for each layer
  , nextObjectId     :: Int                     -- ^ Auto-increments for each placed object
  , orientation      :: Text                    -- ^ @orthogonal@, @isometric@, @staggered@ or @hexagonal@
  , parallaxOriginX  :: Maybe Double            -- ^ X coordinate of the parallax origin in pixels (default: 0)
  , parallaxOriginY  :: Maybe Double            -- ^ Y coordinate of the parallax origin in pixels (default: 0)
  , properties       :: Maybe (Vector Property) -- ^ Array of Properties
  , renderOrder      :: Maybe Text              -- ^ @right-down@ (the default), @right-up@, @left-down@ or @left-up@ (currently only supported for orthogonal maps)
  , staggerAxis      :: Maybe Text              -- ^ x or y (staggered / hexagonal maps only)
  , staggerIndex     :: Maybe Text              -- ^ odd or even (staggered / hexagonal maps only)
  , tiledVersion     :: Text                    -- ^ The Tiled version used to save the file
  , tileHeight       :: Int                     -- ^ Map grid height
  , tilesets         :: Vector TilesetRef       -- ^ Array of Tilesets
  , tileWidth        :: Int                     -- ^ Map grid width
  , type_            :: Text                    -- ^ @map@
  , version          :: Text                    -- ^ The JSON format version
  , width            :: Int                     -- ^ Number of tile columns
  }
  deriving (Eq, Show, Generic)

instance FromJSON Map where
  parseJSON = genericParseJSON

instance ToJSON Map where
  toJSON = genericToJSON
