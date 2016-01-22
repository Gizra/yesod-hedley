-- State.hs
{-# LANGUAGE TemplateHaskell #-}
module State where

import Database.Persist.TH
import Prelude

data GroupMembershipState = Blocked | Pending | Active
    deriving (Show, Read, Eq)

derivePersistField "GroupMembershipState"
