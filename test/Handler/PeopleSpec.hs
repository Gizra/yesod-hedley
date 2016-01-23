module Handler.PeopleSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getPeopleR" $ do
        it "gives a 200" $ do
            get $ EventR eid
            statusIs 200
