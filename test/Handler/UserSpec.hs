module Handler.UserSpec (spec) where

import TestImport
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = withApp .
    describe "getUserR" .
        it "gives a 200" $ do
            uid <- runDB $ insert User
                { userIdent = "foo"
                , userEmail = "foo@example.com"
                , userPassword = Nothing
                , userVerkey = Nothing
                }
            get $ UserR uid
            statusIs 200
