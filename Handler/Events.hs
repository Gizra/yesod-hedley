module Handler.Events where

import qualified Data.Text.Read as T
import Handler.Event
import Import


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


-- Generalize
addOrder :: ( Yesod m
            )
         => [SelectOpt Event]
         -> YesodDB m [ SelectOpt Event ]
addOrder selectOpt = do
  morder <- lookupGetParam "order"
  let order = case morder of
                  Nothing -> Desc EventId
                  Just val -> Asc EventId

  return $ selectOpt `mappend` [order]


getEventsR :: Handler Value
getEventsR = do
    selectOpt <- addPager 5 []
    events <- runDB $
        selectList
            []
            selectOpt :: Handler [Entity Event]

    return $ object ["data" .= events]

postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
