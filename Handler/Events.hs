module Handler.Events where

import           Data.Aeson
import qualified Data.Text           as T  (splitOn)
import qualified Data.Text.Read      as T  (decimal)
import           Handler.Event
import           Import


getCurrentPage :: ( PersistQuery (YesodPersistBackend m)
                  , Yesod m
                  )
               => HandlerT m IO Int
getCurrentPage = do
  mpage <- lookupGetParam "page"
  let pageNumber = case (T.decimal $ fromMaybe "0" mpage) of
                      Left _ -> 0
                      Right (val, _) -> val
  return pageNumber

addPager :: ( PersistEntity val
            , PersistEntityBackend val ~ YesodPersistBackend m
            , PersistQuery (YesodPersistBackend m)
            , Yesod m
            )
         => Int
         -> [ SelectOpt val ]
         -> HandlerT m IO [ SelectOpt val ]
addPager resultsPerPage selectOpt  = do
  pageNumber <- getCurrentPage

  let pagerOpt = [ LimitTo resultsPerPage
                 , OffsetBy $ (pageNumber - 1) * resultsPerPage
                 ]
  return $ selectOpt `mappend` pagerOpt


-- @todo: Generalize not to be only for Event
addOrder :: ( PersistQuery (YesodPersistBackend m)
            , Yesod m
            )
         => [SelectOpt Event]
         -> HandlerT m IO [ SelectOpt Event ]
addOrder selectOpt = do
  morder <- lookupGetParam "order"

  let order = case morder of
                  Nothing -> [ Desc EventId ]
                  Just vals -> orderText2SelectOpt $ T.splitOn "," vals

  return $ selectOpt `mappend` order

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

  return $ keyValues `mappend` metaData


orderText2SelectOpt :: [Text] -> [SelectOpt Event]
orderText2SelectOpt []              = []
orderText2SelectOpt ("id" : xs)     = [ Asc EventId] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("-id" : xs)    = [ Desc EventId] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("title" : xs)  = [ Asc EventTitle] ++ (orderText2SelectOpt xs)
orderText2SelectOpt ("-title" : xs) = [ Desc EventTitle] ++ (orderText2SelectOpt xs)
orderText2SelectOpt (_ : xs)        = [] ++ (orderText2SelectOpt xs)

getEventsR :: Handler Value
getEventsR = do
    selectOpt <- (addPager 2) >=> addOrder $ []
    events <- runDB $ selectList [] selectOpt :: Handler [Entity Event]

    eventsWithMetaData <- addListMetaData ["data" .= events]
    return $ object eventsWithMetaData

postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
