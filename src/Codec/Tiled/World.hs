module Codec.Tiled.World where

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (mkOptions, remapFields_)
import Codec.Tiled.World.Map (Map)
import Codec.Tiled.World.Pattern (Pattern)

data World = World
  { maps                 :: Maybe (Vector Map)
  , patterns             :: Maybe (Vector Pattern)
  , type_                :: Maybe Text              -- ^ @world@ (for world files)
  , onlyShowAdjacentMaps :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON World where
  parseJSON = genericParseJSON (mkOptions remapFields_)

instance ToJSON World where
  toJSON = genericToJSON (mkOptions remapFields_)
