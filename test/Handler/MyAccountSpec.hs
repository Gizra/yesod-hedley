module Handler.MyAccountSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMyAccountR" $ do
        it "redirects anon user" $ do
            get MyAccountR
            statusIs 303
