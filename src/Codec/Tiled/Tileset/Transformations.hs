module Codec.Tiled.Tileset.Transformations where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Transformations = Transformations
  { hflip               :: Maybe Bool -- ^ Tiles can be flipped horizontally
  , vflip               :: Maybe Bool -- ^ Tiles can be flipped vertically
  , rotate              :: Maybe Bool -- ^ Tiles can be rotated in 90-degree increments
  , preferuntransformed :: Maybe Bool -- ^ Whether untransformed tiles remain preferred, otherwise transformed tiles are used to produce more variations
  }
  deriving (Eq, Show, Generic)

instance FromJSON Transformations where
  parseJSON = genericParseJSON

instance ToJSON Transformations where
  toJSON = genericToJSON
