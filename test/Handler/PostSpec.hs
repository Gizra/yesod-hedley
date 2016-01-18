module Handler.PostSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "get single Post" $ do
        it "get single post index" $ do
            pid <- runDB $ insert $ Post "title" "body"
            get $ PostR pid
            statusIs 200


    -- describe "putPostR" $ do
    --     error "Spec not implemented: putPostR"
    --
    --
    -- describe "deletePostR" $ do
    --     error "Spec not implemented: deletePostR"
