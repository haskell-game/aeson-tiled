module Codec.Tiled.Object.Text where

import Data.Text qualified as The
import GHC.Generics (Generic)

import Codec.Tiled.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)

data Text = Text
  { bold       :: Maybe Bool     -- ^  Whether to use a bold font (default: false)
  , color      :: Maybe The.Text -- ^  Hex-formatted color (#RRGGBB or #AARRGGBB) (default: #000000)
  , fontFamily :: Maybe The.Text -- ^  Font family (default: sans-serif)
  , hAlign     :: Maybe The.Text -- ^  Horizontal alignment (center, right, justify or left (default))
  , italic     :: Maybe Bool     -- ^  Whether to use an italic font (default: false)
  , kerning    :: Maybe Bool     -- ^  Whether to use kerning when placing characters (default: true)
  , pixelSize  :: Maybe Int      -- ^  Pixel size of font (default: 16)
  , strikeout  :: Maybe Bool     -- ^  Whether to strike out the text (default: false)
  , text       :: The.Text       -- ^  Text
  , underline  :: Maybe Bool     -- ^  Whether to underline the text (default: false)
  , vAlign     :: Maybe The.Text -- ^  Vertical alignment (center, bottom or top (default))
  , wrap       :: Maybe Bool     -- ^  Whether the text is wrapped within the object bounds (default: false)
  }
  deriving (Eq, Show, Generic)

instance FromJSON Text where
  parseJSON = genericParseJSON

instance ToJSON Text where
  toJSON = genericToJSON
