module Handler.Posts where

import Handler.Post
import Import

getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [] [ Desc PostId ] :: Handler [Entity Post]

    return $ object ["data" .= posts]

postPostsR :: Handler Value
postPostsR = do
    post <- requireJsonBody :: Handler Post
    pid <- runDB $ insert post

    returnVal <- getPostR pid

    sendResponseStatus status201 returnVal
