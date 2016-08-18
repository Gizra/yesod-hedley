-- State.hs
{-# LANGUAGE TemplateHaskell #-}
module State where

import Database.Persist.TH
import Prelude
import Data.Aeson

data GroupMembershipState = Blocked | Pending | Active
    deriving (Show, Eq, Enum, Bounded, Read)

-- @todo: Find why we can't use "derive Generic" on GroupMembershipState.
instance ToJSON (GroupMembershipState) where
  toJSON State.Blocked = "blocked"
  toJSON State.Pending = "pending"
  toJSON State.Active = "active"

derivePersistField "GroupMembershipState"
