module Handler.Events where

import Handler.Event
import Import

getEventsR :: Handler Value
getEventsR = do
    events <- runDB $ selectList [] [ Desc EventId ] :: Handler [Entity Event]

    return $ object ["data" .= events]

postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
