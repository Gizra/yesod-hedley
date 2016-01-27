module Handler.User where

import Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist.Sql (toSqlKey)

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId

    companies <- runDB
           $ E.select
           $ E.from $ \(company `E.InnerJoin` groupMembership) -> do
                E.on $ company ^. CompanyId E.==. groupMembership ^. GroupMembershipCompanyId
                E.where_ $ groupMembership ^. GroupMembershipUserId E.==. E.val userId
                E.limit 5
                return
                    ( company ^. CompanyTitle
                    , groupMembership ^. GroupMembershipState
                    )

    defaultLayout $ do
        setTitle $ toHtml $ userIdent user ++ "'s User page"
        $(widgetFile "user")
