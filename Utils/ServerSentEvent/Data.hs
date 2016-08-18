module Utils.ServerSentEvent.Data
  ( SseEventName(..)
  ) where

import Import.NoFoundation

data SseEventName = AddMembership | RemoveMembership
    deriving (Show, Eq, Enum, Bounded, Read)

instance ToJSON (SseEventName) where
    toJSON AddMembership = "AddMembership"
    toJSON RemoveMembership = "RemoveMembership"
