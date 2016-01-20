module Handler.Event where

import Import
import Data.HashMap.Strict as HashMap (insert)

getEventR :: EventId -> Handler Value
getEventR eid = do
    event <- runDB $ get404 eid

    render <- getUrlRender
    let renderedUrl = render $ EventR eid

    let returnVal = object
          [ "data" .= [buildEntityWithLink (Entity eid event) renderedUrl]]

    return returnVal


buildEntityWithLink :: Entity Event -> Text -> Maybe Value
buildEntityWithLink entity renderedUrl =
    case toJSON entity of
        Object obj ->
            let links = object ["self" .= renderedUrl]
                entityWithLink = HashMap.insert "_links" links obj
            in Just (Object entityWithLink)
        _ -> Nothing

putEventR :: EventId -> Handler Value
putEventR eid = do
    event <- requireJsonBody :: Handler Event

    runDB $ replace eid event

    sendResponseStatus status204 ()

deleteEventR :: EventId -> Handler Value
deleteEventR eid = do
    runDB $ delete eid

    sendResponseStatus status204 ()
