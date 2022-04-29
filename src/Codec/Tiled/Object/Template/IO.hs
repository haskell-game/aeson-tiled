module Codec.Tiled.Object.Template.IO
  ( TemplateError(..)
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

import Codec.Tiled.Object.Template (Template)

newtype TemplateError = TemplateError Text
  deriving (Eq, Show)

instance Exception TemplateError

readFile :: MonadIO m => FilePath -> m Template
readFile source = liftIO do
  bytes <- ByteString.readFile source
  case decodeMap bytes of
    Left msg ->
      throwIO $ TemplateError (Text.pack msg)
    Right res ->
      pure res

decodeMap :: ByteString -> Either String Template
decodeMap = Aeson.eitherDecodeStrict'

writeFile :: MonadIO m => FilePath -> Template -> m ()
writeFile destination = liftIO . Aeson.encodeFile destination
