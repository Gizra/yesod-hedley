module Handler.EventsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .

    describe "get Events index" .
      it "get Events index" $ do
        userId <- runDB $ insert User
            { userIdent = "foo"
            , userEmail = "foo@example.com"
            , userPassword = Nothing
            , userVerkey = Nothing
            }

        currentTime <- liftIO getCurrentTime
        _ <- runDB . insert $ AccessToken currentTime userId "someRandomToken"

        request $ do
            setMethod "GET"
            addGetParam "access_token" "someRandomToken"
            setUrl EventsR


        statusIs 200


    -- describe "postEventsR" $ do
    --     error "Spec not implemented: postEventsR"
