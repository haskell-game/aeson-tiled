{- | Global Tile IDs

https://doc.mapeditor.org/en/latest/reference/global-tile-ids/

-}

module Data.Tiled.GID where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Bits (Bits(..), (.&.))
import Data.Bool (bool)
import Data.Word (Word8, Word32)
import Foreign (Storable(..))
import GHC.Generics (Generic)

newtype GID = GID { getGID :: Word32 }
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Storable, Bits)

pattern FLIPPED_HORIZONTALLY_BIT :: Int
pattern FLIPPED_HORIZONTALLY_BIT = 31

pattern FLIPPED_VERTICALLY_BIT :: Int
pattern FLIPPED_VERTICALLY_BIT = 30

pattern ROTATED_60_BIT :: Int
pattern ROTATED_60_BIT = 29

pattern ROTATED_120_BIT :: Int
pattern ROTATED_120_BIT = 28

data Flags = Flags
  { flippedHorizontally :: Bool
  , flippedVertically   :: Bool
  , rotated60           :: Bool
  , rotated120          :: Bool
  } deriving (Eq, Show, Generic)

{-# INLINEABLE flags #-}
flags :: GID -> Flags
flags = unpack32 . getGID

{-# INLINEABLE pack32 #-}
pack32 :: Flags -> Word32
pack32 Flags{..} =
  bool 0 (bit FLIPPED_HORIZONTALLY_BIT) flippedHorizontally .|.
  bool 0 (bit FLIPPED_VERTICALLY_BIT)   flippedVertically .|.
  bool 0 (bit ROTATED_60_BIT)           rotated60 .|.
  bool 0 (bit ROTATED_120_BIT)          rotated120

{-# INLINEABLE unpack32 #-}
unpack32 :: Word32 -> Flags
unpack32 flags32 = Flags
  { flippedHorizontally = testBit flags32 FLIPPED_HORIZONTALLY_BIT
  , flippedVertically   = testBit flags32 FLIPPED_VERTICALLY_BIT
  , rotated60           = testBit flags32 ROTATED_60_BIT
  , rotated120          = testBit flags32 ROTATED_120_BIT
  }

{-# INLINEABLE pack8 #-}
pack8 :: Flags -> Word8
pack8 flags = fromIntegral (pack32 flags `shiftR` 24)

{-# INLINEABLE unpack8 #-}
unpack8 :: Word8 -> Flags
unpack8 bits8 = unpack32 (fromIntegral bits8 `shiftL` 24)

noFlags :: Flags
noFlags = Flags
  { flippedHorizontally = False
  , flippedVertically   = False
  , rotated60           = False
  , rotated120          = False
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
fromLocal flags localId =
  GID $
    fromIntegral localId .|.
    pack32 flags
