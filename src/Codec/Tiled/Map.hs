module Codec.Tiled.Map where

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Layer (Layer)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset (Tileset)

data Map = Map
  { backgroundcolor  :: Maybe Text      -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
  , compressionlevel :: Int             -- ^ The compression level to use for tile layer data (defaults to -1, which means to use the algorithm default)
  , height           :: Int             -- ^ Number of tile rows
  , hexsidelength    :: Int             -- ^ Length of the side of a hex tile in pixels (hexagonal maps only)
  , infinite         :: Bool            -- ^ Whether the map has infinite dimensions
  , layers           :: Vector Layer    -- ^ Array of Layers
  , nextlayerid      :: Int             -- ^ Auto-increments for each layer
  , nextobjectid     :: Int             -- ^ Auto-increments for each placed object
  , orientation      :: Text            -- ^ orthogonal, isometric, staggered or hexagonal
  , parallaxoriginx  :: Double          -- ^ X coordinate of the parallax origin in pixels (since 1.8, default: 0)
  , parallaxoriginy  :: Double          -- ^ Y coordinate of the parallax origin in pixels (since 1.8, default: 0)
  , properties       :: Vector Property -- ^ Array of Properties
  , renderorder      :: Text            -- ^ right-down (the default), right-up, left-down or left-up (currently only supported for orthogonal maps)
  , staggeraxis      :: Text            -- ^ x or y (staggered / hexagonal maps only)
  , staggerindex     :: Text            -- ^ odd or even (staggered / hexagonal maps only)
  , tiledversion     :: Text            -- ^ The Tiled version used to save the file
  , tileheight       :: Int             -- ^ Map grid height
  , tilesets         :: Vector Tileset  -- ^ Array of Tilesets
  , tilewidth        :: Int             -- ^ Map grid width
  , type_            :: Text            -- ^ @map@ (since 1.0)
  , version          :: Text            -- ^ The JSON format version (previously a number, saved as string since 1.6)
  , width            :: Int             -- ^ Number of tile columns
  }
  deriving (Eq, Show, Generic)

instance FromJSON Map where
  parseJSON = genericParseJSON

instance ToJSON Map where
  toJSON = genericToJSON
