module Handler.Events where

import           Data.Aeson
import qualified Data.Text           as T  (splitOn)
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
            Just vals -> orderText2SelectOpt $ T.splitOn "," vals

getTotalCount :: ( YesodPersist site
                 , YesodPersistBackend site ~ SqlBackend
                 )
              => [Filter Event]
              -> HandlerT site IO Int
getTotalCount filters =
  runDB $ count (filters :: [Filter Event])

addListMetaData :: KeyValue t
                => [t]
                -> HandlerT App IO ([t])
addListMetaData keyValues = do
  render <- getUrlRender
  totalCount <- getTotalCount []

  let metaData =
        [ "self" .= render EventsR
        , "count" .= totalCount
        ]

  return $ keyValues ++ metaData


orderText2SelectOpt :: [Text] -> [SelectOpt Event]
orderText2SelectOpt []              = []
orderText2SelectOpt ("id" : xs)     = [ Asc EventId] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("-id" : xs)    = [ Desc EventId] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("title" : xs)  = [ Asc EventTitle] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("-title" : xs) = [ Desc EventTitle] ++ (orderText2SelectOpt xs)
orderText2SelectOpt (_ : xs)        = [] ++ (orderText2SelectOpt xs)

getEventsR :: Handler Value
getEventsR = do
    mpage <- lookupGetParam "page"
    morder <- lookupGetParam "order"

    -- selectOpt <- (addPager mpage 2) >=> addOrder $ []
    let selectOpt = (addPager mpage 2) . (addOrder morder) $ []
    events <- runDB $ selectList [] selectOpt :: Handler [Entity Event]

    urlRender <- getUrlRender

    let maybeEvents = [addMetaData urlRender eid event | Entity eid event <- events]

    eventsWithMetaData <- addListMetaData ["data" .= maybeEvents]
    return $ object eventsWithMetaData


postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
