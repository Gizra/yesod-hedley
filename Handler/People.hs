module Handler.People where

import Import

import Yesod.Table
import qualified Yesod.Table as Table
import Utils.Table (buildSemanticUi)
import Yesod.Paginator


getPeopleR :: Handler Html
getPeopleR = do
    (people, pagerWidget) <- runDB $ selectPaginated 2 [] [ Desc UserId ] :: Handler ([Entity User],  WidgetT App IO ())

    let table = buildSemanticUi peopleTable people
    defaultLayout $ do
        setTitle "People List"
        $(widgetFile "people")

peopleTable :: Table App (Entity User)
peopleTable = mempty
    ++ Table.linked "Username" (userIdent . entityVal) (UserR . entityKey)
    ++ Table.text   "Email"    (userEmail . entityVal)
