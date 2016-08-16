{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Subsite.ServerSentEvent.Data where

import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent.Chan
import           Data.Aeson.Encode                   (encode)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Yesod

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data ServerSentEvent = ServerSentEvent (Chan ServerEvent)

mkYesodSubData "ServerSentEvent" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

class (Yesod master, RenderMessage master FormMessage)
        => YesodServerSentEvent master where
    isLoggedIn :: HandlerT master IO Bool


type ServerSentEventHandler a =
    forall master. YesodServerSentEvent master =>
    HandlerT ServerSentEvent (HandlerT master IO) a

sendMessage app eventName eventId msg = do
  ServerSentEvent chan <- app
  liftIO $ writeChan chan
      $ ServerEvent (Just $ fromText eventName) (Just $ fromText eventId)
      $ return $ encode msg

getReceiveR :: ServerSentEventHandler ()
getReceiveR = do
    ServerSentEvent chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    sendWaiApplication $ eventSourceAppChan chan
