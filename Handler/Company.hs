module Handler.Company where

import Import

getCompanyR :: CompanyId -> Handler Html
getCompanyR companyId = do
    company <- runDB $ get404 companyId
    defaultLayout $ do
        setTitle . toHtml $ (companyTitle company ) `mappend` " | Company"
        $(widgetFile "company")
