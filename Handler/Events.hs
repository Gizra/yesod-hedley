module Handler.Events where

import Import

getEventsR :: EventId -> Handler Value
getEventsR eventId = do
    event <- runDB $ get404 eventId
    returnJson event
