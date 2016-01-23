module Handler.People where

import Import

import Yesod.Table (Table)
import qualified Yesod.Table as Table

getPeopleR :: Handler Html
getPeopleR = do
    people <- runDB $ selectList [] [ Desc UserId ] :: Handler [Entity User]

    let ppl = map entityVal people

    defaultLayout $ do
        setTitle "People List"
        Table.buildBootstrap peopleTable ppl

peopleTable :: Table site User
peopleTable = mempty
    ++ Table.text   "Username"      userIdent
    -- ++ Table.text   "Email (todo)"  userPassword
