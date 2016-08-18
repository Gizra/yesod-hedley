module Utils.Companies where

import           Import
import qualified Database.Esqueleto   as E
import           Database.Esqueleto      ((^.))

getUserCompanies :: UserId -> Handler Widget
getUserCompanies userId = do
    _ <- runDB $ get404 userId

    companies <- runDB
           . E.select
           . E.from $ \(company `E.InnerJoin` groupMembership) -> do
                E.on $ company ^. CompanyId E.==. groupMembership ^. GroupMembershipCompanyId
                E.where_ $ groupMembership ^. GroupMembershipUserId E.==. E.val userId
                E.limit 5
                return
                    ( company ^. CompanyTitle
                    , company ^. CompanyId
                    , groupMembership ^. GroupMembershipState
                    )

    return $ $(widgetFile "companies")
