module Blockchain.UI.RestApi.Server (
  bootstrap
) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BS
import           Data.Function((&))
import           Data.Text (unpack)
import           Network.HostName (getHostName)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai (responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import           Servant ((:<|>)(..), Application, Server, serve, serveDirectoryWith)
import           WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import           WaiAppStatic.Types (StaticSettings(..))

import           Blockchain.UI.Config (UiConfig(..))
import           Blockchain.UI.Service.Server (UiService(..), newUiServiceHandle)
import           Blockchain.UI.RestApi (UiApi, uiApi)
import           Logger
import           RestUtil (convert)

infoL :: (MonadIO m) => String -> m ()
[infoL] = getLogger "Blockchain.UI.RestApi.Server" [INFO]

server :: UiConfig -> UiService IO -> Server UiApi
server cfg service = toApi getNodes
                :<|> toApi mine
                :<|> toApi getTransactions
                :<|> toApi newTransaction
                :<|> toApi getAccountsList
                :<|> serveDirectoryWith dirConfig
                where
                  toApi method = convert (service & method)
                  dirConfig = (defaultWebAppSettings staticPath) {
                    ss404Handler = Just (redirect404ToIndex staticPath)
                  }
                  staticPath = unpack $ staticFilesPath cfg


redirect404ToIndex :: String -> Application
redirect404ToIndex path _ respond = do
  body <- BS.readFile (path ++ "/index.html")
  respond $ responseLBS status200 [] body


-- | Starts the web application
bootstrap :: UiConfig -> IO ()
bootstrap config = do
  hostname <- getHostName
  infoL "~~~~ Starting UI server ~~~~"
  infoL $ show config
  let settings =
        setPort (httpPort config) $
        setBeforeMainLoop (infoL $ "listening on endpoint http://" ++ hostname ++ ":" ++ show (httpPort config)) $
        defaultSettings
  runSettings settings =<< (makeApplication config)


makeApplication :: UiConfig -> IO Application
makeApplication config = do
  uiServiceHandle <- newUiServiceHandle config
  return $ serve uiApi (server config uiServiceHandle)
