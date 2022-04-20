module Codec.Tiled.Object where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Object.Point (Point)
import Codec.Tiled.Object.Text qualified as Object (Text)
import Codec.Tiled.Property (Property)
import Data.Tiled.GID (GID)

data Object = Object
  { ellipse    :: Maybe Bool              -- ^ Used to mark an object as an ellipse
  , gid        :: Maybe GID               -- ^ Global tile ID, only if object represents a tile
  , height     :: Double                  -- ^ Height in pixels.
  , id         :: Int                     -- ^ Incremental ID, unique across all objects
  , name       :: Text                    -- ^ String assigned to name field in editor
  , point      :: Maybe Bool              -- ^ Used to mark an object as a point
  , polygon    :: Maybe (Vector Point)    -- ^ Array of Points, in case the object is a polygon
  , polyline   :: Maybe (Vector Point)    -- ^ Array of Points, in case the object is a polyline
  , properties :: Maybe (Vector Property) -- ^ Array of Properties
  , rotation   :: Double                  -- ^ Angle in degrees clockwise
  , template   :: FilePath                -- ^ Reference to a template file, in case object is a template instance
  , text       :: Maybe Object.Text       -- ^ Only used for text objects
  , type_      :: Text                    -- ^ String assigned to type field in editor
  , visible    :: Bool                    -- ^ Whether object is shown in editor.
  , width      :: Double                  -- ^ Width in pixels.
  , x          :: Double                  -- ^ X coordinate in pixels
  , y          :: Double                  -- ^ Y coordinate in pixels
  }
  deriving (Eq, Show, Generic)

instance FromJSON Object where
  parseJSON = genericParseJSON

instance ToJSON Object where
  toJSON = genericToJSON
