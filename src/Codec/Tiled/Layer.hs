module Codec.Tiled.Layer
  ( Layer(..)
  , empty

  , pattern TILELAYER
  , pattern OBJECTGROUP
  , pattern IMAGELAYER
  , pattern GROUP
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Layer.Chunk (Chunk)
import Codec.Tiled.Layer.Data (LayerData)
import Codec.Tiled.Object (Object)
import Codec.Tiled.Property (Property)

data Layer = Layer
  { chunks           :: Maybe (Vector Chunk)    -- ^ Array of chunks (optional). @tilelayer@ only.
  , compression      :: Maybe Text              -- ^ @zlib@, @gzip@, @zstd@ or empty (default). @tilelayer@ only.
  , data_            :: Maybe LayerData         -- ^ Array of unsigned int (GIDs) or base64-encoded data. @tilelayer@ only.
  , draworder        :: Maybe Text              -- ^ @topdown@ (default) or @index@. @objectgroup@ only.
  , encoding         :: Maybe Text              -- ^ @csv@ (default) or @base64@. @tilelayer@ only.
  , height           :: Maybe Int               -- ^ Row count. Same as map height for fixed-size maps.
  , id               :: Maybe Int               -- ^ Incremental ID - unique across all layers
  , image            :: Maybe FilePath          -- ^ Image used by this layer. @imagelayer@ only.
  , layers           :: Maybe (Vector Layer)    -- ^ Array of layers. @group@ only.
  , locked           :: Maybe Bool              -- ^ Whether layer is locked in the editor (default: false). (since Tiled 1.8.2)
  , name             :: Text                    -- ^ Name assigned to this layer
  , objects          :: Maybe (Vector Object)   -- ^ Array of objects. @objectgroup@ only.
  , offsetX          :: Maybe Double            -- ^ Horizontal layer offset in pixels (default: 0)
  , offsetY          :: Maybe Double            -- ^ Vertical layer offset in pixels (default: 0)
  , opacity          :: Double                  -- ^ Value between 0 and 1.
  , parallaxX        :: Maybe Double            -- ^ Horizontal parallax factor for this layer (default: 1).
  , parallaxY        :: Maybe Double            -- ^ Vertical parallax factor for this layer (default: 1).
  , properties       :: Maybe (Vector Property) -- ^ Array of Properties
  , repeatX          :: Maybe Bool              -- ^ Whether the image drawn by this layer is repeated along the X axis. @imagelayer@ only.
  , repeatY          :: Maybe Bool              -- ^ Whether the image drawn by this layer is repeated along the Y axis. @imagelayer@ only.
  , startX           :: Maybe Int               -- ^ X coordinate where layer content starts (for infinite maps)
  , startY           :: Maybe Int               -- ^ Y coordinate where layer content starts (for infinite maps)
  , tintColor        :: Maybe Text              -- ^ Hex-formatted tint color (#RRGGBB or #AARRGGBB) that is multiplied with any graphics drawn by this layer or any child layers (optional).
  , transparentColor :: Maybe Text              -- ^ Hex-formatted color (#RRGGBB) (optional). @imagelayer@ only.
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

empty  :: Layer
empty  = Layer
  { chunks           = Nothing
  , compression      = Nothing
  , data_            = Nothing
  , draworder        = Nothing
  , encoding         = Nothing
  , height           = Nothing
  , id               = Nothing
  , image            = Nothing
  , layers           = Nothing
  , locked           = Nothing
  , name             = ""
  , objects          = Nothing
  , offsetX          = Nothing
  , offsetY          = Nothing
  , opacity          = 1.0
  , parallaxX        = Nothing
  , parallaxY        = Nothing
  , properties       = Nothing
  , repeatX          = Nothing
  , repeatY          = Nothing
  , startX           = Nothing
  , startY           = Nothing
  , tintColor        = Nothing
  , transparentColor = Nothing
  , type_            = ""
  , visible          = True
  , width            = Nothing
  , x                = Nothing
  , y                = Nothing
  }

pattern TILELAYER :: Text
pattern TILELAYER = "tilelayer"

pattern OBJECTGROUP :: Text
pattern OBJECTGROUP = "objectgroup"

pattern IMAGELAYER :: Text
pattern IMAGELAYER = "imagelayer"

pattern GROUP :: Text
pattern GROUP = "group"
