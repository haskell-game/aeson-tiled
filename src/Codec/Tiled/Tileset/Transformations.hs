module Codec.Tiled.Tileset.Transformations
  ( Transformations(..)
  , empty
  ) where

import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Transformations = Transformations
  { hFlip               :: Maybe Bool -- ^ Tiles can be flipped horizontally
  , vFlip               :: Maybe Bool -- ^ Tiles can be flipped vertically
  , rotate              :: Maybe Bool -- ^ Tiles can be rotated in 90-degree increments
  , preferUntransformed :: Maybe Bool -- ^ Whether untransformed tiles remain preferred, otherwise transformed tiles are used to produce more variations
  }
  deriving (Eq, Show, Generic)

instance FromJSON Transformations where
  parseJSON = genericParseJSON

instance ToJSON Transformations where
  toJSON = genericToJSON

empty :: Transformations
empty = Transformations
  { hFlip               = Nothing
  , vFlip               = Nothing
  , rotate              = Nothing
  , preferUntransformed = Nothing
  }
