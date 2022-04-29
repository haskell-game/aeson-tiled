module Codec.Tiled.Tileset.WangSet where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Codec.Tiled.Property (Property)
import Codec.Tiled.Tileset.WangColor (WangColor)
import Codec.Tiled.Tileset.WangTile (WangTile)

data WangSet = WangSet
  { colors     :: Vector WangColor        -- ^ Array of Wang colors (since 1.5)
  , name       :: Text                    -- ^ Name of terrain
  , properties :: Maybe (Vector Property) -- ^ array Array of Properties
  , tile       :: Int                     -- ^ Local ID of tile representing terrain
  , type_      :: Text                    -- ^ @corner@, @edge@ or @mixed@ (since 1.5)
  , wangTiles  :: Vector WangTile         -- ^ Array of Wang tiles
  }
  deriving (Eq, Show, Generic)

instance FromJSON WangSet where
  parseJSON = genericParseJSON

instance ToJSON WangSet where
  toJSON = genericToJSON
