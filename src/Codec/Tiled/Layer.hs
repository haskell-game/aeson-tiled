module Codec.Tiled.Layer where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Layer.Chunk (Chunk)
import Codec.Tiled.Layer.Data (LayerData)
import Codec.Tiled.Object (Object)
import Codec.Tiled.Property (Property)

data Layer = Layer
  { chunks           :: Maybe (Vector Chunk)    -- ^ Array of chunks (optional). tilelayer only.
  , compression      :: Maybe Text              -- ^ @zlib@, @gzip@, @zstd@ or empty (default). tilelayer only.
  , data_            :: Maybe LayerData         -- ^ Array of unsigned int (GIDs) or base64-encoded data. tilelayer only.
  , draworder        :: Maybe Text              -- ^ @topdown@ (default) or @index@. objectgroup only.
  , encoding         :: Maybe Text              -- ^ @csv@ (default) or @base64@. tilelayer only.
  , height           :: Maybe Int               -- ^ Row count. Same as map height for fixed-size maps.
  , id               :: Maybe Int               -- ^ Incremental ID - unique across all layers
  , image            :: Maybe FilePath          -- ^ Image used by this layer. imagelayer only.
  , layers           :: Maybe (Vector Layer)    -- ^ Array of layers. group only.
  , locked           :: Maybe Bool              -- ^ Whether layer is locked in the editor (default: false). (since Tiled 1.8.2)
  , name             :: Text                    -- ^ Name assigned to this layer
  , objects          :: Maybe (Vector Object)   -- ^ Array of objects. objectgroup only.
  , offsetx          :: Maybe Double            -- ^ Horizontal layer offset in pixels (default: 0)
  , offsety          :: Maybe Double            -- ^ Vertical layer offset in pixels (default: 0)
  , opacity          :: Double                  -- ^ Value between 0 and 1.
  , parallaxx        :: Maybe Double            -- ^ Horizontal parallax factor for this layer (default: 1).
  , parallaxy        :: Maybe Double            -- ^ Vertical parallax factor for this layer (default: 1).
  , properties       :: Maybe (Vector Property) -- ^ Array of Properties
  , repeatx          :: Maybe Bool              -- ^ Whether the image drawn by this layer is repeated along the X axis. imagelayer only.
  , repeaty          :: Maybe Bool              -- ^ Whether the image drawn by this layer is repeated along the Y axis. imagelayer only.
  , startx           :: Maybe Int               -- ^ X coordinate where layer content starts (for infinite maps)
  , starty           :: Maybe Int               -- ^ Y coordinate where layer content starts (for infinite maps)
  , tintcolor        :: Maybe Text              -- ^ Hex-formatted tint color (#RRGGBB or #AARRGGBB) that is multiplied with any graphics drawn by this layer or any child layers (optional).
  , transparentcolor :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB) (optional). imagelayer only.
  , type_            :: Text                    -- ^ @tilelayer@, @objectgroup@, @imagelayer@ or @group@
  , visible          :: Bool                    -- ^ Whether layer is shown or hidden in editor
  , width            :: Maybe Int               -- ^ Column count. Same as map width for fixed-size maps.
  , x                :: Maybe Int               -- ^ Horizontal layer offset in tiles. Always 0.
  , y                :: Maybe Int               -- ^ Vertical layer offset in tiles. Always 0.
  }
  deriving (Eq, Show, Generic)

instance FromJSON Layer where
  parseJSON = genericParseJSON

instance ToJSON Layer where
  toJSON = genericToJSON
