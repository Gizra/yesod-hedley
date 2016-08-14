module Handler.WebSockets where

import Import
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Data.Time
import Conduit

timeSource :: MonadIO m => Source m TL.Text
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

getWebSocketsR :: Handler Html
getWebSocketsR = do
    webSockets $ race_
        (sourceWS $$ mapC TL.toUpper =$ sinkWSText)
        (timeSource $$ sinkWSText)
    defaultLayout $
        toWidget
            [julius|
                var conn = new WebSocket("ws://localhost:3000/websockets");
                conn.onopen = function() {
                    document.write("<p>open!</p>");
                    document.write("<button id=button>Send another message</button>")
                    document.getElementById("button").addEventListener("click", function(){
                        var msg = prompt("Enter a message for the server");
                        conn.send(msg);
                    });
                    conn.send("hello world");
                };
                conn.onmessage = function(e) {
                    document.write("<p>" + e.data + "</p>");
                };
            |]
