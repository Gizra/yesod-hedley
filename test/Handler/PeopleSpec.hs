module Handler.PeopleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .

    describe "getPeopleR" .
        it "gives a 200" $ do
            get PeopleR
            statusIs 200
