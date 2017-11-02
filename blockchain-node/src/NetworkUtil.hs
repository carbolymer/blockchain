module NetworkUtil (
    isValidUrl
  , toBaseUrl
) where

import           Control.Monad.Catch (MonadCatch, catch, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text, unpack)
import           Servant.Common.BaseUrl (BaseUrl(..), InvalidBaseUrlException, parseBaseUrl)


isValidUrl :: (MonadIO m) => Text -> m Bool
isValidUrl url = liftIO $ catch
    (parseBaseUrl (unpack url) >> return True)
    handler
    where
      handler :: InvalidBaseUrlException -> IO Bool
      handler _ = return False


toBaseUrl :: (MonadCatch m) => Text -> m (Either InvalidBaseUrlException BaseUrl)
toBaseUrl = try . parseBaseUrl . unpack
