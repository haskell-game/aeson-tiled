module Codec.Tiled.Tileset.Tile
  ( Tile(..)
  , empty
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Layer (Layer)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset.Frame (Frame)

data Tile = Tile
  { animation   :: Maybe (Vector Frame)    -- ^ Array of Frames
  , id          :: Int                     -- ^ Local ID of the tile
  , image       :: Maybe FilePath          -- ^ Image representing this tile (optional)
  , imageHeight :: Int                     -- ^ Height of the tile image in pixels
  , imageWidth  :: Int                     -- ^ Width of the tile image in pixels
  , objectGroup :: Maybe Layer             -- ^ Layer with type objectgroup, when collision shapes are specified (optional)
  , probability :: Maybe Double            -- ^ Percentage chance this tile is chosen when competing with others in the editor (optional)
  , properties  :: Maybe (Vector Property) -- ^ Array of Properties
  , terrain     :: Maybe (Vector Int)      -- ^ Index of terrain for each corner of tile (optional)
  , type_       :: Maybe Text              -- ^ The type of the tile (optional)
  }
  deriving (Eq, Show, Generic)

instance FromJSON Tile where
  parseJSON = genericParseJSON

instance ToJSON Tile where
  toJSON = genericToJSON

empty :: Tile
empty = Tile
  { animation   = Nothing
  , id          = 0
  , image       = Nothing
  , imageHeight = 0
  , imageWidth  = 0
  , objectGroup = Nothing
  , probability = Nothing
  , properties  = Nothing
  , terrain     = Nothing
  , type_       = Nothing
  }
