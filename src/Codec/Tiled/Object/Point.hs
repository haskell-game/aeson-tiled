module Codec.Tiled.Object.Point where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Point = Point
  { x :: Double -- ^ X coordinate in pixels
  , y :: Double -- ^ Y coordinate in pixels
  }
  deriving (Eq, Show, Generic)

instance FromJSON Point where
  parseJSON = genericParseJSON

instance ToJSON Point where
  toJSON = genericToJSON
