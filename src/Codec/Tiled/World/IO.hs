module Codec.Tiled.World.IO
  ( WorldError(..)
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

import Codec.Tiled.World (World)

newtype WorldError = WorldError Text
  deriving (Eq, Show)

instance Exception WorldError

readFile :: MonadIO m => FilePath -> m World
readFile source = liftIO do
  bytes <- ByteString.readFile source
  case decodeMap bytes of
    Left msg ->
      throwIO $ WorldError (Text.pack msg)
    Right res ->
      pure res

decodeMap :: ByteString -> Either String World
decodeMap = Aeson.eitherDecodeStrict'

writeFile :: MonadIO m => FilePath -> World -> m ()
writeFile destination = liftIO . Aeson.encodeFile destination
