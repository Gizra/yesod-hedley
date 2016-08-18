module Handler.SseReceiveSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getSseReceiveR" $ do
      it "gives a 403" $ do
          get SseReceiveR
          statusIs 403
