module Handler.MyAccount where

import Import
import qualified Database.Esqueleto   as E
import           Database.Esqueleto      ((^.))

getMyAccountR :: Handler Html
getMyAccountR = do
    (userId, user) <- requireAuthPair

    companies <- runDB
           . E.select
           . E.from $ \(company `E.InnerJoin` groupMembership) -> do
                E.on $ company ^. CompanyId E.==. groupMembership ^. GroupMembershipCompanyId
                E.where_ $ groupMembership ^. GroupMembershipUserId E.==. E.val userId
                E.limit 5
                return
                    ( company ^. CompanyTitle
                    , groupMembership ^. GroupMembershipState
                    )

    defaultLayout $ do
        setTitle . toHtml $ userIdent user `mappend` "'s User page"
        $(widgetFile "user")
