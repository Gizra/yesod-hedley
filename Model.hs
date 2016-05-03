{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Sql (fromSqlKey)

import Database.Persist.Quasi
import State
import Yesod.Auth.Email

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")



instance ToJSON (Entity Event) where
    toJSON (Entity eid e) = object
        [ "id"    .= (fromSqlKey eid)
        , "title" .= eventTitle e
        , "user"  .= eventUserId e
        ]

instance FromJSON Event where
    parseJSON (Object o) = Event
        <$> o .: "title"
        <*> o .: "content"
        <*> o .: "user"

    parseJSON _ = mzero
