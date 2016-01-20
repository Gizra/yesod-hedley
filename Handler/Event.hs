module Handler.Event where

import Import

getEventR :: EventId -> Handler Value
getEventR pid = do
    post <- runDB $ get404 pid

    render <- getUrlRender
    let renderedUrl = render $ EventR pid

    let links = object
          [ "self" .= renderedUrl
          ]

    let returnVal = object
          [ "data" .= (Entity pid post)
          , "_links" .= links
          ]

    return returnVal

putEventR :: EventId -> Handler Value
putEventR pid = do
    post <- requireJsonBody :: Handler Event

    runDB $ replace pid post

    sendResponseStatus status204 ()

deleteEventR :: EventId -> Handler Value
deleteEventR pid = do
    runDB $ delete pid

    sendResponseStatus status204 ()
