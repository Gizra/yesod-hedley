module Handler.SseReceive where

import Import
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Aeson.Encode (encode)
import Network.Wai.EventSource

getSseReceiveR :: Handler Html
getSseReceiveR = do
    chan <- fmap appServerEvent getYesod
    duppedChan <- dupChan chan
    sendWaiApplication $ eventSourceAppChan duppedChan

-- sendMessage :: Text -> Text -> Text
sendMessage eventName eventId msg = do
    chan <- fmap appServerEvent getYesod
    liftIO $ writeChan chan
        $ ServerEvent (Just $ fromText eventName) (Just $ fromText eventId)
        $ return $ fromText msg
