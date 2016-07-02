module Handler.Event where

import           Data.Aeson
import           Data.Aeson.Types           (Value( String ))
import           Database.Persist.Sql       (fromSqlKey)
import qualified Data.HashMap.Strict  as HM (insert)
import           Import



-- @todo: Use [(ParamName, ParamValue)] instead of String from Yesod.Request
addMetaData :: (Route App -> [(Text, Text)] -> Text)
            -> [(Text, Text)]
            -> EventId
            -> Event
            -> Maybe (HashMap Text Value)
addMetaData urlRenderParams params eid event =
    case toJSON (Entity eid event) of
        Object obj -> Just $ HM.insert "self" self obj
        _          -> Nothing

    where self = String $ urlRenderParams (EventR eid) params

getEventR :: EventId -> Handler Value
getEventR eid = do
    event <- runDB $ get404 eid
    urlRenderParams <- getUrlRenderParams
    params <- reqGetParams <$> getRequest
    let eventWithMetaData = addMetaData urlRenderParams params eid event

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
