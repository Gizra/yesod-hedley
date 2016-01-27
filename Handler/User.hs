module Handler.User where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId

    let authorID = toSqlKey 1 :: AuthorId -- cast 1 to the expected key
    companies <- runDB
           $ E.select
           $ E.from $ \(company `E.InnerJoin` groupMembership) -> do
                E.on $ company ^. CompanyUserId E.==. groupMembership ^. GroupMembershipUserId
                E.where_ $ company ^. CompanyUserId E.=. authorID -- how to pass ID 1
                E.limit 5
                return
                    ( company ^. CompanyId
                    , company ^. CompanyTitle
                    , groupMembership ^. GroupMembershipState
                    )

    defaultLayout $ do
        setTitle $ toHtml $ userIdent user ++ "'s User page"
        $(widgetFile "user")
