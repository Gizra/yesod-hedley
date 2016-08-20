module Utils.Menu where

import           Import

data MenuItem = MenuItem
  { _menuItemLabel :: Text
  , _menuItemAnonymous :: Bool
  , _menuItemAuthentictated :: Bool
  , _menuItemRoute :: Route App
  } deriving (Show)

data Menu = LeftMenu MenuItem | RightMenu MenuItem

getMenuItems :: [Menu]
getMenuItems =
  [ LeftMenu $ MenuItem
    { _menuItemLabel = "Home"
    , _menuItemAnonymous = True
    , _menuItemAuthentictated = True
    , _menuItemRoute = HomeR
    }

  ]

getMenuWidget :: Handler Html
getMenuWidget = do
  defaultLayout
      [whamlet|
        Menu will be here
      |]
