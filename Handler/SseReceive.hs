module Handler.SseReceive where

import Import
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Aeson.Encode (encodeToBuilder)
import Network.Wai.EventSource

getSseReceiveR :: Handler ()
getSseReceiveR = do
    chan <- fmap appServerEvent getYesod
    duppedChan <- dupChan chan
    sendWaiApplication $ eventSourceAppChan duppedChan
