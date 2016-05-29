module Handler.Events where

import           Data.Aeson
import           Data.Attoparsec.Text as AT  (maybeResult, parse, string, takeTill, Parser)
import qualified Data.Text            as T   (append, isPrefixOf, pack, splitOn, tail, unpack)
import qualified Data.Text.Read       as T   (decimal)
import           Database.Persist.Sql        (toSqlKey)
import           Handler.Event
import           Import


getCurrentPage :: Maybe Text -> Either Text Int
getCurrentPage mpage =
    case (T.decimal $ fromMaybe "0" mpage) of
        Left _ -> Left $ T.pack "Invalid page ID"
        Right (val, _) -> Right val

addPager :: Maybe Text
         -> Int
         -> [ SelectOpt Event ]
         -> Either Text [ SelectOpt Event ]
addPager mpage resultsPerPage selectOpt =
    case getCurrentPage mpage of
        Left val -> Left val
        Right pageNumber ->
            Right $ selectOpt ++ pagerOpt
            where pagerOpt = [ LimitTo resultsPerPage
                             , OffsetBy $ (pageNumber - 1) * resultsPerPage
                             ]

addOrder :: Maybe Text
         -> [SelectOpt Event]
         -> Either Text [ SelectOpt Event ]
addOrder morder selectOpt = do
    case order of
      Right val -> Right $ selectOpt ++ val
      Left val -> Left val

    where order = case morder of
            Nothing -> Right [ Desc EventId ]
            Just vals -> textToSelectOptList $ T.splitOn "," vals


filterParser :: AT.Parser Text
filterParser = do
    _ <- AT.string "filter["
    filterKey <- AT.takeTill (== ']')
    _ <- AT.string "]"
    return filterKey


addFilter :: Maybe Text
         -> [Filter Event]
         -> Either Text [ Filter Event ]
addFilter mfilter filters = do
    case mfilter of
      Just vals -> textToFilterList $ T.splitOn "," vals
      Nothing      -> Right []

textToFilter :: Text -> Either Text (Filter Event)
textToFilter text =
    case text of
        "id"    -> Right $ EventId >. toSqlKey 3
        "title" -> Right $ EventTitle ==. "post 4"
        "user"  -> Right $ EventUserId >. toSqlKey 2
        _       -> Left $ T.pack "invalid filter"

instance Monoid (Either Text [Filter Event]) where
  mempty = Left mempty
  mappend (Right a) (Right b) = Right $ a ++ b
  mappend (Left a) (_) = Left a
  mappend (_) (Left b) = Left b


textToFilterList :: [Text] -> Either Text [Filter Event]
textToFilterList []       = Right []
textToFilterList (x : xs) = case textToFilter x of
                                Right val -> (Right [ val ]) `mappend` (textToFilterList xs)
                                Left val  -> Left val


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
    mpage   <- lookupGetParam "page"
    morder  <- lookupGetParam "order"

    let selectOpt = case addPager mpage 3 >=> addOrder morder $ [] of
                        Right val -> val
                        Left val  -> error $ T.unpack val

    params <- reqGetParams <$> getRequest

    let filterParams = mapMaybe (\(queryParam, filterValue) -> case AT.maybeResult $ AT.parse filterParser queryParam of     -- (2) (4)
                    Nothing -> Nothing
                    Just filterKey -> Just (filterKey, filterValue)
                  ) params
    -- liftIO $ print . "Params are " ++ show filterParams

    let filters = case addFilter mfilter [] of
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
