module Codec.Tiled.Tileset.IO
  ( TilesetError(..)
  , readFile
  , writeFile
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text

import Codec.Tiled.Tileset (Tileset)

newtype TilesetError = TilesetError Text
  deriving (Eq, Show)

instance Exception TilesetError

readFile :: MonadIO m => FilePath -> m Tileset
readFile source = liftIO do
  bytes <- ByteString.readFile source
  case decodeMap bytes of
    Left msg ->
      throwIO $ TilesetError (Text.pack msg)
    Right res ->
      pure res

decodeMap :: ByteString -> Either String Tileset
decodeMap = Aeson.eitherDecodeStrict'

writeFile :: MonadIO m => FilePath -> Tileset -> m ()
writeFile destination = liftIO . Aeson.encodeFile destination
