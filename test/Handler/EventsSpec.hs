module Handler.EventsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .

    describe "get Events index" .
      it "get Events index" $ do
        request $ do
            setMethod "GET"
            addGetParam "access_token" "1234"
            setUrl EventsR


        statusIs 200


    -- describe "postEventsR" $ do
    --     error "Spec not implemented: postEventsR"
