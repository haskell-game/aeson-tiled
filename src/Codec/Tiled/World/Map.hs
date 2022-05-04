module Codec.Tiled.World.Map where

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

data Map = Map
  { fileName :: FilePath
  , x        :: Int -- ^ Global position X, in pixels
  , y        :: Int -- ^ Global position Y, in pixels
  , width    :: Maybe Int
  , height   :: Maybe Int
  }
  deriving (Eq, Show, Generic)

-- XXX: don't transform, fileName field
instance FromJSON Map
instance ToJSON Map
