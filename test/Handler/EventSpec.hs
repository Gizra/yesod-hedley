module Handler.EventSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "get single Event" $ do
        it "get single event index" $ do
            eid <- runDB $ insert $ Event "title" "body"
            get $ EventR eid
            statusIs 200


    -- describe "putEventR" $ do
    --     error "Spec not implemented: putEventR"
    --
    --
    -- describe "deleteEventR" $ do
    --     error "Spec not implemented: deleteEventR"
