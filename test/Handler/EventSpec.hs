module Handler.EventSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .
    describe "get single Event" .
        it "get single event index" $ do
            userId <- runDB $ insert User
                { userIdent = "foo"
                , userEmail = "foo@example.com"
                , userPassword = Nothing
                , userVerkey = Nothing
                }

            currentTime <- liftIO getCurrentTime

            _ <- runDB . insert $ AccessToken currentTime userId "someRandomToken"

            eid <- runDB . insert $ Event "title" "body" userId

            request $ do
                setMethod "GET"
                setUrl $ EventR eid
                addGetParam "access_token" "someRandomToken"
            statusIs 200


    -- describe "putEventR" $ do
    --     error "Spec not implemented: putEventR"
    --
    --
    -- describe "deleteEventR" $ do
    --     error "Spec not implemented: deleteEventR"
