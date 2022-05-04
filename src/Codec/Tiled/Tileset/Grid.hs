module Codec.Tiled.Tileset.Grid where

import Data.Text (Text)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Grid = Grid
  { height      :: Int        -- ^ Cell height of tile grid
  , orientation :: Maybe Text -- ^ orthogonal (default) or isometric
  , width       :: Int        -- ^ Cell width of tile grid
  }
  deriving (Eq, Show, Generic)

instance FromJSON Grid where
  parseJSON = genericParseJSON

instance ToJSON Grid where
  toJSON = genericToJSON
