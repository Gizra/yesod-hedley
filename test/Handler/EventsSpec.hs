module Handler.EventsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp .

    describe "get Events index" .
      it "get Events index" $ do
          get EventsR
          statusIs 200


    -- describe "postEventsR" $ do
    --     error "Spec not implemented: postEventsR"
