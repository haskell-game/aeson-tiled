module Data.Tiled.Loader
  ( readMap
  , MapError(..)
  , decodeMap
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Control.Exception (Exception, throwIO)

import Codec.Tiled.Map (Map)

newtype MapError = MapError Text
  deriving (Eq, Show)

instance Exception MapError

readMap :: MonadIO m => FilePath -> m Map
readMap source = liftIO do
  bytes <- ByteString.readFile source
  case decodeMap bytes of
    Left msg ->
      throwIO $ MapError (Text.pack msg)
    Right res ->
      pure res

decodeMap :: ByteString -> Either String Map
decodeMap = Aeson.eitherDecodeStrict'
