module Handler.MyAccountSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMyAccountR" $ do
        it "gives a 200" $ do
            get MyAccountR
            statusIs 200
