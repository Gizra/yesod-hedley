module Handler.User where

import Import

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId

    defaultLayout $ do
        setTitle $ toHtml $ userIdent user ++ "'s User page"
        $(widgetFile "user")
