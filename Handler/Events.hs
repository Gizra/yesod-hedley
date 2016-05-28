module Handler.Events where

import           Data.Aeson
import qualified Data.Text           as T  (isPrefixOf, splitOn, tail)
import qualified Data.Text.Read      as T  (decimal)
import           Handler.Event
import           Import


getCurrentPage :: Maybe Text -> Int
getCurrentPage mpage =
    case (T.decimal $ fromMaybe "0" mpage) of
        Left _ -> 0
        Right (val, _) -> val

addPager :: Maybe Text
         -> Int
         -> [ SelectOpt val ]
         -> [ SelectOpt val ]
addPager mpage resultsPerPage selectOpt =
    selectOpt ++ pagerOpt
    where pageNumber = getCurrentPage mpage
          pagerOpt = [ LimitTo resultsPerPage
                     , OffsetBy $ (pageNumber - 1) * resultsPerPage
                     ]


-- @todo: Generalize not to be only for Event
addOrder :: Maybe Text
         -> [SelectOpt Event]
         -> [ SelectOpt Event ]
addOrder morder selectOpt = do
    selectOpt ++ order

    where order = case morder of
            Nothing -> [ Desc EventId ]
            Just vals -> textToSelectOptList $ T.splitOn "," vals

getTotalCount :: ( YesodPersist site
                 , YesodPersistBackend site ~ SqlBackend
                 )
              => [Filter Event]
              -> HandlerT site IO Int
getTotalCount filters =
  runDB $ count (filters :: [Filter Event])

addListMetaData :: KeyValue t
                => (Route App -> Text)
                -> Int
                -> [t]
                -> [t]
addListMetaData urlRender totalCount keyValues =
    keyValues ++ metaData

    where metaData =
            [ "self" .= urlRender EventsR
            , "count" .= totalCount
            ]


textToSelectOpt :: Text -> SelectOpt Event
textToSelectOpt text =
    case textWithNoPrefix of
        "id"    -> direction text $ EventId
        "title" -> direction text $ EventTitle
        "user"  -> direction text $ EventUserId
        _       -> error "Wrong order"

    where textWithNoPrefix = if T.isPrefixOf "-" text
                then T.tail text
                else text
          direction t = if T.isPrefixOf "-" t
                    then Desc
                    else Asc



textToSelectOptList :: [Text] -> [SelectOpt Event]
textToSelectOptList []       = []
textToSelectOptList (x : xs) = [ textToSelectOpt x ] ++ (textToSelectOptList xs)

getEventsR :: Handler Value
getEventsR = do
    mpage <- lookupGetParam "page"
    morder <- lookupGetParam "order"

    let selectOpt = (addPager mpage 2) . (addOrder morder) $ []

    events <- runDB $ selectList [] selectOpt :: Handler [Entity Event]

    urlRender <- getUrlRender
    let maybeEvents = [addMetaData urlRender eid event | Entity eid event <- events]

    totalCount <- getTotalCount []

    let eventsWithMetaData = addListMetaData urlRender totalCount ["data" .= maybeEvents]
    return $ object eventsWithMetaData


postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
