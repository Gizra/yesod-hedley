module Handler.Posts where

import Import

getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [] [ Desc PostId ] :: Handler [Entity Post]

    return $ object ["data" .= posts]

postPostsR :: Handler Value
postPostsR = do
    post <- requireJsonBody :: Handler Post
    pid <- runDB $ insert post

    return $ object ["data" .= (Entity pid post)]
    -- sendResponseStatus status201 $ object ["data" .= (Post "foo" "barzz")]
