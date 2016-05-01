module Handler.Event where

import Import



getEventR :: EventId -> Handler Value
getEventR eid = do
    event <- runDB $ get404 eid
    return $ object ["data" .= (Entity eid event)]


putEventR :: EventId -> Handler Value
putEventR eid = do
    event <- requireJsonBody :: Handler Event

    runDB $ replace eid event

    sendResponseStatus status204 ()

deleteEventR :: EventId -> Handler Value
deleteEventR eid = do
    runDB $ delete eid

    sendResponseStatus status204 ()
