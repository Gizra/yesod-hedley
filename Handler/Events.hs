module Handler.Events where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T  (splitOn)
import qualified Data.Text.Read      as T
import           Handler.Event
import           Import


addPager :: ( PersistEntity val
            , PersistEntityBackend val ~ YesodPersistBackend m
            , PersistQuery (YesodPersistBackend m)
            , Yesod m
            )
         => Int
         -> [ SelectOpt val ]
         -> HandlerT m IO [ SelectOpt val ]
addPager resultsPerPage selectOpt  = do
  mpage <- lookupGetParam "page"
  let pageNumber = case (T.decimal $ fromMaybe "0" mpage) of
                      Left _ -> 0
                      Right (val, _) -> val
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


orderText2SelectOpt :: [Text] -> [SelectOpt Event]
orderText2SelectOpt []           = []
orderText2SelectOpt ("id" : xs)  = [ Asc EventId] ++ (orderFunc xs)
orderText2SelectOpt ("-id" : xs) = [ Desc EventId] ++ (orderFunc xs)
orderText2SelectOpt ("title" : xs)  = [ Asc EventTitle] ++ (orderFunc xs)
orderText2SelectOpt ("-title" : xs) = [ Desc EventTitle] ++ (orderFunc xs)
orderText2SelectOpt (_ : xs)     = [] ++ (orderFunc xs)

getEventsR :: Handler Value
getEventsR = do
    selectOpt <- (addPager 5) [] >>= addOrder
    events <- runDB $ selectList [] selectOpt :: Handler [Entity Event]

    return $ object ["data" .= events]

postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
