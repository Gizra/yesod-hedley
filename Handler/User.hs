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
                E.on $ company ^. CompanyUserId E.==. groupMembership ^. GroupMembershipUserId
                E.where_ $ company ^. CompanyUserId E.==. E.val userId
                E.limit 5
                return
                    ( company ^. CompanyId
                    , company ^. CompanyTitle
                    , groupMembership ^. GroupMembershipState
                    )

    defaultLayout $ do
        setTitle $ toHtml $ userIdent user ++ "'s User page"
        $(widgetFile "user")
