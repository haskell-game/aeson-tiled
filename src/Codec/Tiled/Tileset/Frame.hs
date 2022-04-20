module Codec.Tiled.Tileset.Frame where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Frame = Frame
  { duration :: Int -- ^ Frame duration in milliseconds
  , tileid   :: Int -- ^ Local tile ID representing this frame
  }
  deriving (Eq, Show, Generic)

instance FromJSON Frame where
  parseJSON = genericParseJSON

instance ToJSON Frame where
  toJSON = genericToJSON
