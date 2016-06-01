module Handler.MyAccountSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .
    describe "getMyAccountR" .
        it "redirects anon user" $ do
            get MyAccountR
            statusIs 303
