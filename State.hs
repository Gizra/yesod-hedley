-- State.hs
{-# LANGUAGE TemplateHaskell #-}
module State where

import Database.Persist.TH
import Prelude

data GroupMembershipState = Blocked | Pending | Active
    deriving (Show, Eq, Enum, Bounded, Read)

derivePersistField "GroupMembershipState"
