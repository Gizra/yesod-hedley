module Handler.EventSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "get single Event" $ do
        it "get single event index" $ do
            uid <- runDB $ insert $ User "foo" $ Just "bar"
            eid <- runDB $ insert $ Event "title" "body" uid
            get $ EventR eid
            statusIs 200


    -- describe "putEventR" $ do
    --     error "Spec not implemented: putEventR"
    --
    --
    -- describe "deleteEventR" $ do
    --     error "Spec not implemented: deleteEventR"