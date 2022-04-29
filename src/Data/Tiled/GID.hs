{- | Global Tile IDs

https://doc.mapeditor.org/en/latest/reference/global-tile-ids/

-}

module Data.Tiled.GID where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Bits (Bits(..), (.&.))
import Data.Bool (bool)
import Data.Word (Word32)
import Foreign (Storable(..))
import GHC.Generics (Generic)

newtype GID = GID { getGID :: Word32 }
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Storable, Bits)

pattern FLIPPED_HORIZONTALLY_BIT :: Int
pattern FLIPPED_HORIZONTALLY_BIT = 32

pattern FLIPPED_VERTICALLY_BIT :: Int
pattern FLIPPED_VERTICALLY_BIT = 31

pattern ROTATED_60_BIT :: Int
pattern ROTATED_60_BIT = 30

pattern ROTATED_120_BIT :: Int
pattern ROTATED_120_BIT = 29

data Flags = Flags
  { flippedHorizontally :: Bool
  , flippedVertically   :: Bool
  , rotated60           :: Bool
  , rotated120          :: Bool
  } deriving (Eq, Show, Generic)

flags :: GID -> Flags
flags (GID bits) = Flags
  { flippedHorizontally = testBit bits FLIPPED_HORIZONTALLY_BIT
  , flippedVertically   = testBit bits FLIPPED_VERTICALLY_BIT
  , rotated60           = testBit bits ROTATED_60_BIT
  , rotated120          = testBit bits ROTATED_120_BIT
  }

flagBits :: Word32
flagBits =
  bit FLIPPED_HORIZONTALLY_BIT .|.
  bit FLIPPED_VERTICALLY_BIT .|.
  bit ROTATED_60_BIT .|.
  bit ROTATED_120_BIT

toLocal :: GID -> Int
toLocal (GID bits) = fromIntegral $ bits .&. complement flagBits

fromLocal :: Flags -> Int -> GID
fromLocal Flags{..} localId =
  GID . fromIntegral $
    localId
    .|. bool 0 FLIPPED_HORIZONTALLY_BIT flippedHorizontally
    .|. bool 0 FLIPPED_VERTICALLY_BIT   flippedVertically
    .|. bool 0 ROTATED_60_BIT           rotated60
    .|. bool 0 ROTATED_120_BIT          rotated120
