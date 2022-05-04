module Codec.Tiled.World.Pattern where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)

data Pattern = Pattern
  { regexp      :: Text
  , mapHeight   :: Maybe Int
  , mapWidth    :: Maybe Int
  , multiplierX :: Int
  , multiplierY :: Int
  , offsetX     :: Int
  , offsetY     :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Pattern
instance ToJSON Pattern
