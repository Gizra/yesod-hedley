module Handler.SseReceive where

import           Import
import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Data.Aeson.Encode                   (encode)
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream

getSseReceiveR :: Handler Html
getSseReceiveR = do
    chan0 <- fmap appServerEvent getYesod
    chan <- liftIO $ dupChan chan0
    sendWaiApplication $ eventSourceAppChan chan

-- sendMessage :: Text -> Text -> Text
sendMessage eventName eventId msg = do
    chan <- fmap appServerEvent getYesod
    liftIO $ writeChan chan
        $ ServerEvent (Just $ fromText eventName) (Just $ fromText eventId)
        $ return $ fromText msg


-- toasterWidget :: Widget
toasterWidget = do
    [julius|
        // Set up the receiving end.
        var src = new EventSource("@{getSseReceiveR}");

        src.addEventListener('wiki-update', function(msg) {
          updateServerSentEvent(msg);
        });

        src.onmessage = function(msg) {
          updateServerSentEvent(msg);
        };

        updateServerSentEvent = function(msg) {
          // Msg can now be easily parsed.
          var data = JSON.parse(msg.data);
          console.log(data);
        };
    |]
