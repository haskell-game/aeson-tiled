module Data.Tiled.GID where

import Data.Word (Word32)
import Foreign (Storable(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..))

newtype GID = GID { getGID :: Word32 }
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Storable)
