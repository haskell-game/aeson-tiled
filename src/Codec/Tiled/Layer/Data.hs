module Codec.Tiled.Layer.Data where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), withArray, withText)
import Data.Text (Text)
import Data.Vector.Storable qualified as Storable
import GHC.Generics (Generic)

import Data.Tiled.GID (GID)

data LayerData
  = Base64 Text
  | GIDs (Storable.Vector GID)
  deriving (Eq, Show, Generic)

instance FromJSON LayerData where
  parseJSON v = encoded v <|> csv v
    where
      encoded =
        withText "Base64" $
          pure . Base64

      csv =
        withArray "GIDs" $
          fmap (GIDs . Storable.convert) . traverse parseJSON

instance ToJSON LayerData where
  toJSON = \case
    Base64 text ->
      toJSON text
    GIDs ints ->
      toJSON ints
