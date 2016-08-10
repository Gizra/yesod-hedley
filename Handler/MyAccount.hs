module Handler.MyAccount where

import Import
import Utils.Companies

getMyAccountR :: Handler Html
getMyAccountR = do
    (userId, user) <- requireAuthPair
    companiesWidget <- getUserCompanies userId

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
