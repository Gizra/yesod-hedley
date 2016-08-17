module Handler.SseReceive where

import Import
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Aeson.Encode (encodeToBuilder)
import Network.Wai.EventSource
--
-- data Chat = Chat (Chan ServerEvent)
--
-- type ChatHandler a =
--     HandlerT Chat a

getSseReceiveR :: Handler ()
getSseReceiveR = do
    chan <- fmap appServerEvent getYesod
    duppedChan <- dupChan chan
    sendWaiApplication $ eventSourceAppChan duppedChan

sendMessage :: ToJSON a => Text -> Text -> a -> Handler ()
sendMessage eventName eventId msg = do
    chan <- fmap appServerEvent getYesod
    liftIO $ writeChan chan
        $ ServerEvent (Just $ fromText eventName) (Just $ fromText eventId)
        $ return $ encodeToBuilder $ toJSON msg
