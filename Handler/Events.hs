module Handler.Events where

import qualified Data.Text.Read as T
import Handler.Event
import Import

getEventsR :: Handler Value
getEventsR = do
    let resultsPerPage = 1
    mpage <- lookupGetParam "page"
    let pageNumber = case (T.decimal $ fromMaybe "0" mpage) of
                        Left _ -> 0
                        Right (val, _) -> val
    events <- runDB $
        selectList
            []
            [ Desc EventId
            , LimitTo resultsPerPage
            , OffsetBy $ (pageNumber - 1) * resultsPerPage
            ] :: Handler [Entity Event]

    return $ object ["data" .= events]

postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
