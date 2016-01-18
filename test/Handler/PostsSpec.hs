module Handler.PostsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "get Posts index" $ do
      it "get Posts index" $ do
          get PostsR
          statusIs 200


    -- describe "postPostsR" $ do
    --     error "Spec not implemented: postPostsR"
