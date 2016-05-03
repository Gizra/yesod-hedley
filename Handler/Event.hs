module Handler.Event where

import           Data.Aeson
import           Data.Aeson.Types           (Value( String ))
import           Database.Persist.Sql       (fromSqlKey)
import qualified Data.HashMap.Strict  as HM (insert)
import           Import


addMetaData :: EventId
            -> Event
            -> HandlerT App IO (Maybe (HashMap Text Value))
addMetaData eid event = do
    render <- getUrlRender
    let self = String . render $ EventR eid
    let result =
          case toJSON (Entity eid event) of
              Object obj -> Just $ HM.insert "self" self obj
              _          -> Nothing

    return $ result


getEventR :: EventId -> Handler Value
getEventR eid = do
    event <- runDB $ get404 eid
    eventWithMetaData <- addMetaData eid event

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
