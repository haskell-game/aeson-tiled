module Codec.Tiled.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , genericParseJSON
  , genericToJSON
  , mkOptions
  , remapFields
  , remapFields_
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
genericParseJSON = Aeson.genericParseJSON (mkOptions remapFields)

genericToJSON
  :: ( Generic a
     , Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)
     )
  => a
  -> Aeson.Value
genericToJSON = Aeson.genericToJSON (mkOptions remapFields)

mkOptions :: (String -> String) -> Aeson.Options
mkOptions fieldMods = Aeson.defaultOptions
  { Aeson.fieldLabelModifier  = fieldMods
  , Aeson.omitNothingFields   = True
  , Aeson.rejectUnknownFields = False
  }

-- | Drop trailing @_@ and convert field names to lowercase.
remapFields :: String -> String
remapFields = remapFields_ . map toLower

-- | Only drop trailing @_@.
remapFields_ :: String -> String
remapFields_ = dropWhileEnd (== '_')
