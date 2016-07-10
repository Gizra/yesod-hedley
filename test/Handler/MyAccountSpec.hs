module Handler.MyAccountSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .
    describe "getMyAccountR" .
        it "Access denied anon user" $ do
            get MyAccountR
            statusIs 403
