module Utils.Menu where

import           Import

data MenuItem = MenuItem
  { _menuItemLabel :: Text
  , _menuItemAnonymous :: Bool
  , _menuItemAuthentictated :: Bool
  , _menuItemRoute :: Route App
  } deriving (Show)

data Menu = LeftMenu MenuItem | RightMenu MenuItem
  deriving (Show)

getMenuItems :: [Menu]
getMenuItems =
  [ LeftMenu $ MenuItem
    { _menuItemLabel = "Home"
    , _menuItemAnonymous = False
    , _menuItemAuthentictated = True
    , _menuItemRoute = HomeR
    }
  , LeftMenu $ MenuItem
    { _menuItemLabel = "People"
    , _menuItemAnonymous = False
    , _menuItemAuthentictated = True
    , _menuItemRoute = PeopleR
    }
  ]

getMenuWidget :: Handler Html
getMenuWidget = do
  currentRoute <- getCurrentRoute
  defaultLayout $
     mconcat $ fmap getMenuItem getMenuItems



getMenuItem :: Menu -> Widget
getMenuItem menu =
  case menu of
    LeftMenu menuItem ->
      [whamlet|
        <a class="active item" href="@{_menuItemRoute menuItem}">#{_menuItemLabel menuItem}
      |]
    RightMenu _ ->
      error "Implement me"
