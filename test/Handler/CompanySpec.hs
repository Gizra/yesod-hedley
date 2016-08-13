module Handler.CompanySpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "get single Company" .
      it "get single company index" $ do
          userId <- runDB $ insert User
              { userIdent = "foo"
              , userEmail = "foo@example.com"
              , userPassword = Nothing
              , userVerkey = Nothing
              }

          cid <- runDB . insert $ Company "title" userId

          get $ CompanyR cid
          statusIs 200
