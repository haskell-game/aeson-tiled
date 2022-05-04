module Codec.Tiled.Tileset.Ref where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)

import Codec.Tiled.Tileset (Tileset)
import Data.Tiled.GID (GID)

data TilesetRef
  = TilesetRef
      { firstGid :: GID      -- ^ GID corresponding to the first tile in the set
      , source   :: FilePath -- ^ The external file that contains this tilesets data
      }
  | TilesetEmbedded
      { firstGid :: GID
      , embedded :: Tileset
      }
  deriving (Eq, Show, Generic)

instance FromJSON TilesetRef where
  parseJSON v = refP v <|> embeddedP v
    where
      refP = Aeson.withObject "TilesetRef" \o -> do
        source <- o .: "source"
        firstGid <- o .: "firstgid"
        pure TilesetRef{..}

      embeddedP = Aeson.withObject "TilesetEmbedded" \o -> do
        embedded <- parseJSON (Aeson.Object o)
        firstGid <- o .: "firstgid"
        pure TilesetEmbedded{..}

instance ToJSON TilesetRef where
  toJSON = \case
    TilesetRef{..} -> Aeson.object
      [ "firstgid" .= firstGid
      , "source"   .= source
      ]
    TilesetEmbedded{..} ->
      case toJSON embedded of
        Aeson.Object o ->
          Aeson.Object $
            KeyMap.insert "firstGid" (toJSON firstGid) o
        _nonObject ->
          error "assert: TilesetRef is Object"
