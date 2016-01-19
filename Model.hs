{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


-- { "id": 1, "title": "A title", "content": "The content" }
instance ToJSON (Entity Event) where
    toJSON (Entity eid e) = object
        [ "id"      .= (String $ toPathPiece eid)
        , "title"   .= eventTitle e
        , "content" .= eventContent e
        ]

instance FromJSON Event where
    parseJSON (Object o) = Event
        <$> o .: "title"
        <*> o .: "content"

    parseJSON _ = mzero
