module Codec.Tiled.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , genericParseJSON
  , genericToJSON
  )
  where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.List (dropWhileEnd)
import GHC.Generics (Generic, Rep)

genericParseJSON
  :: ( Generic a
     , Aeson.GFromJSON Aeson.Zero (Rep a)
     )
  => Aeson.Value
  -> Parser a
genericParseJSON = Aeson.genericParseJSON (options remapFields)

genericToJSON
  :: ( Generic a
     , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
     )
  => a
  -> Aeson.Value
genericToJSON = Aeson.genericToJSON (options remapFields)

options :: (String -> String) -> Aeson.Options
options fieldMods = Aeson.defaultOptions
  { Aeson.fieldLabelModifier  = fieldMods
  , Aeson.omitNothingFields   = True
  , Aeson.rejectUnknownFields = False
  }

remapFields :: String -> String
remapFields = dropWhileEnd (== '_') . map toLower
