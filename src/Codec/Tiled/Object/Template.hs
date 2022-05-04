module Codec.Tiled.Object.Template where

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Codec.Tiled.Object (Object)
import Codec.Tiled.Tileset (Tileset)

data Template = Template
  { type_   :: Text          -- ^ template
  , tileset :: Maybe Tileset -- ^ External tileset used by the template (optional)
  , object  :: Object        -- ^ The object instantiated by this template
  }
  deriving (Eq, Show, Generic)

instance FromJSON Template where
  parseJSON = genericParseJSON

instance ToJSON Template where
  toJSON = genericToJSON
