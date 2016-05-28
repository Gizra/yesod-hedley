module Handler.Events where

import           Data.Aeson
import qualified Data.Text           as T  (append, isPrefixOf, pack, splitOn, tail, unpack)
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

addOrder :: Maybe Text
         -> [SelectOpt Event]
         -> Either Text [ SelectOpt Event ]
addOrder morder selectOpt =
    case order of
      Right val -> Right $ selectOpt ++ val
      Left val -> Left val

    where order = case morder of
            Nothing -> Right [ Desc EventId ]
            Just vals -> textToSelectOptList $ T.splitOn "," vals



addFilters :: Maybe Text
         -> [Filter Event]
         -> Either Text [ Filter Event ]
addFilters mfilter filters =
    Right filters


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


textToSelectOpt :: Text -> Either Text (SelectOpt Event)
textToSelectOpt text =
    case textWithNoPrefix of
        "id"    -> Right . direction text $ EventId
        "title" -> Right . direction text $ EventTitle
        "user"  -> Right . direction text $ EventUserId
        _       -> Left $ T.append textWithNoPrefix (T.pack " is an invalid order")

    where textWithNoPrefix = if T.isPrefixOf "-" text
                then T.tail text
                else text
          direction t = if T.isPrefixOf "-" t
                    then Desc
                    else Asc

instance Monoid (Either Text [SelectOpt Event]) where
  mempty = Left mempty
  mappend (Right a) (Right b) = Right $ a ++ b
  mappend (Left a) (_) = Left a
  mappend (_) (Left b) = Left b


textToSelectOptList :: [Text] -> Either Text [SelectOpt Event]
textToSelectOptList []       = Right []
textToSelectOptList (x : xs) = case textToSelectOpt x of
                                   Right val -> (Right [ val ]) `mappend` (textToSelectOptList xs)
                                   Left val  -> Left val

getEventsR :: Handler Value
getEventsR = do
    mfilter <- lookupGetParam "filter"
    morder  <- lookupGetParam "order"
    mpage   <- lookupGetParam "page"

    let selectOpt = case (addPager mpage 2) <$> addOrder morder [] of
                        Right val -> val
                        Left val  -> error $ T.unpack val


    let filters = case addFilters mfilter [] of
                      Right val -> val
                      Left val  -> error $ T.unpack val

    events <- runDB $ selectList filters selectOpt :: Handler [Entity Event]

    urlRender <- getUrlRender
    let maybeEvents = [addMetaData urlRender eid event | Entity eid event <- events]

    totalCount <- getTotalCount filters

    let eventsWithMetaData = addListMetaData urlRender totalCount ["data" .= maybeEvents]
    return $ object eventsWithMetaData


postEventsR :: Handler Value
postEventsR = do
    event <- requireJsonBody :: Handler Event
    eid <- runDB $ insert event

    returnVal <- getEventR eid

    sendResponseStatus status201 returnVal
