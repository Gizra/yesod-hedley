module Handler.People where

import Import

getPeopleR :: Handler Html
getPeopleR = do
    people <- runDB $ selectList [] [ Desc UserId ] :: Handler [Entity User]

    defaultLayout $ do
        setTitle "People List"
        $(widgetFile "people")
