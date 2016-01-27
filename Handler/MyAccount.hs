module Handler.MyAccount where

import Import

getMyAccountR :: Handler Html
getMyAccountR = do
    (_, user) <- requireAuthPair

    defaultLayout $ do
        setTitle "Account"
        $(widgetFile "my-account")
