module Handler.Event where

import           Data.Aeson
import           Data.Aeson.Types           (Value( String ))
import           Database.Persist.Sql       (fromSqlKey)
import qualified Data.HashMap.Strict  as HM (insert)
import           Import


addMetaData :: (Route App -> Text)
            -> EventId
            -> Event
            -> Maybe (HashMap Text Value)
addMetaData urlRender eid event =
    case toJSON (Entity eid event) of
        Object obj -> Just $ HM.insert "self" self obj
        _          -> Nothing

    where self = String . urlRender $ EventR eid

getEventR :: EventId -> Handler Value
getEventR eid = do
    event <- runDB $ get404 eid
    urlRender <- getUrlRender
    let eventWithMetaData = addMetaData urlRender eid event

    return $ object ["data" .= eventWithMetaData]


putEventR :: EventId -> Handler Value
putEventR eid = do
    event <- requireJsonBody :: Handler Event

    runDB $ replace eid event

    sendResponseStatus status204 ()

deleteEventR :: EventId -> Handler Value
deleteEventR eid = do
    runDB $ delete eid

    sendResponseStatus status204 ()
