module Handler.User where

import Import
import Utils.Companies

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId
    companiesWidget <- getUserCompanies userId

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
