module Handler.Post where

import Import

getPostR :: PostId -> Handler Value
getPostR pid = do
    post <- runDB $ get404 pid

    render <- getUrlRender
    let renderedUrl = render $ PostR pid

    let returnVal = object
          [ "data" .= (Entity pid post)
          , "_self" .= renderedUrl
          ]

    return returnVal

putPostR :: PostId -> Handler Value
putPostR pid = do
    post <- requireJsonBody :: Handler Post

    runDB $ replace pid post

    sendResponseStatus status204 ()

deletePostR :: PostId -> Handler Value
deletePostR pid = do
    runDB $ delete pid

    sendResponseStatus status204 ()
