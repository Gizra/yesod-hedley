module Handler.Company where

import Import

getCompanyR :: CompanyId -> Handler Html
getCompanyR companyId = do
    company <- runDB $ get404 companyId
    let title = companyTitle company
    let authorUserId = companyUserId company
    author <- runDB $ get404 authorUserId
    let authorName = userIdent author
    defaultLayout $ do
        setTitle . toHtml $ (companyTitle company ) `mappend` " | Company"
        $(widgetFile "company")
