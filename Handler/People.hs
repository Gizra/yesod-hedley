module Handler.People where

import Import

import Yesod.Table (Table)
import qualified Yesod.Table as Table
import Yesod.Paginator


getPeopleR :: Handler Html
getPeopleR = do
    (people, pagerWidget) <- runDB $ selectPaginated 2 [] [ Desc UserId ] :: Handler ([Entity User],  WidgetT App IO ())

    let ppl = map entityVal people
    let table = Table.buildBootstrap peopleTable ppl

    defaultLayout $ do
        setTitle "People List"
        $(widgetFile "people")

peopleTable :: Table site User
peopleTable = mempty
    ++ Table.text   "Username"  userIdent
    ++ Table.text   "Email"     userEmail
