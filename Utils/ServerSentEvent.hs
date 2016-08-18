module Utils.ServerSentEvent
  ( sendMessage
  ) where

import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Aeson.Encode (encodeToBuilder)
import Network.Wai.EventSource
import Utils.ServerSentEvent.Data
import Import


sendMessage :: ToJSON a => SseEventName -> Text -> a -> Handler ()
sendMessage eventName eventId msg = do
    chan <- fmap appServerEvent getYesod
    liftIO $ writeChan chan
        $ ServerEvent (Just . encodeToBuilder $ toJSON eventName) (Just $ fromText eventId)
        $ return $ encodeToBuilder $ toJSON msg
