module Handler.Events where

import           Data.Aeson
import           Data.Attoparsec.Text as AT  (maybeResult, parse, string, takeTill, Parser)
import qualified Data.Text            as T   (append, empty, isPrefixOf, pack, splitOn, tail, unpack)
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

getFilterParams :: (Text, Text)
                -> Maybe (Text, Text)
getFilterParams (queryParam, filterValue) =
    case AT.maybeResult $ AT.parse filterParser queryParam of
        Just filterKey -> Just (filterKey, filterValue)
        Nothing -> Nothing

addFilter :: [ (Text, Text) ]
          -> [Filter Event]
          -> Either Text [ Filter Event ]
addFilter []                    filters = Right filters
addFilter (("id"   , key) : xs) filters = filterId           key EventId     "id"    (addFilter xs filters)
addFilter (("title", key) : xs) filters = filterTextRequired key EventTitle  "title" (addFilter xs filters)
addFilter (("user" , key) : xs) filters = filterId           key EventUserId "user"  (addFilter xs filters)
addFilter ((val    , _)   : _)  _       = Left . T.append val $ T.pack " is an invalid filter key"


filterId :: ( PersistField (Key record)
            , ToBackendKey SqlBackend record
            )
         => Text
         -> EntityField Event (Key record)
         -> Text
         -> Either Text [Filter Event]
         -> Either Text [Filter Event]
filterId key filterType name rest =
    case T.decimal key of
        Right (val, _) -> Right [ filterType ==. toSqlKey val ] `mappend` rest
        Left _         -> Left . T.append key $ T.pack " is an invalid value for the " ++ name ++ " filter"


filterTextRequired :: Text
           -> EntityField Event Text
           -> Text
           -> Either Text [Filter Event]
           -> Either Text [Filter Event]
filterTextRequired ""  filterType name rest = Left . T.append name $ T.pack " filter, when used cannot be empty, as the field is required"
filterTextRequired key filterType name rest = Right [ filterType ==. key ] `mappend` rest

instance Monoid (Either Text [Filter Event]) where
  mempty = Left mempty
  mappend (Right a) (Right b) = Right $ a ++ b
  mappend (Left a) (_) = Left a
  mappend (_) (Left b) = Left b


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
    mpage  <- lookupGetParam "page"
    morder <- lookupGetParam "order"
    params <- reqGetParams <$> getRequest

    let selectOpt = case addPager mpage 3 >=> addOrder morder $ [] of
                        Right val -> val
                        Left val  -> error $ T.unpack val

    let filterParams = mapMaybe getFilterParams params

    let filters = case addFilter filterParams [] of
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
