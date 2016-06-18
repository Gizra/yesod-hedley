{-# LANGUAGE QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Auth.Token
    ( -- * Plugin
      authToken
    , YesodAuthToken (..)
    , TokenCreds (..)
      -- * Routes
    , loginR
      -- * Types
    , Token
      -- * Others
    , tokenCreds
    ) where

import Network.Mail.Mime (randomString)
import Yesod.Auth
import System.Random
import qualified Data.Text as TS
import Data.Text (Text)
import Yesod.Core
import qualified Yesod.Auth.Message as Msg
import Yesod.Form

loginR :: AuthRoute
loginR = PluginR "token" ["token_login"]

type Token = Text

-- | Data used to identify the user
data TokenCreds site = TokenCreds
    { tokenCredsAuthId :: AuthId site
    , tokenCredsContent :: Token
    }

class ( YesodAuth site
      , PathPiece (AuthTokenId site)
      , (RenderMessage site Msg.AuthMessage)
      )
  => YesodAuthToken site where
    type AuthTokenId site

    -- | TODO
    setUserToken :: AuthTokenId site -> Token -> HandlerT site IO ()

    -- | TODO
    getTokenCreds :: Token -> HandlerT site IO (Maybe (TokenCreds site))

    -- | Generate a random alphanumeric token.
    randomToken :: site -> IO Token
    randomToken _ = newStdGen >>= return . TS.pack . fst . randomString len
        where len = 32


authToken :: YesodAuthToken m => AuthPlugin m
authToken =
    AuthPlugin "token" dispatch $ \tm ->
        [whamlet|
$newline never
<form method=post action=@{tm loginR}>
    <table>
        <tr>
            <th>
                Token
            <td>
                <input type=text name=token>
        <tr>
            <td colspan=2>
                <button type=submit .btn .btn-success>
                    _{Msg.LoginTitle}
|]
  where
    dispatch "POST" ["token_login"] = postLoginR >>= sendResponse
    dispatch _ _ = notFound

postLoginR :: YesodAuthToken master => HandlerT Auth (HandlerT master IO) TypedContent
postLoginR = do
    token <- lift . runInputPost $ ireq textField "token"
    mCreds <- lift $ getTokenCreds token
    case mCreds of
        Just (TokenCreds _uid _token) ->
            lift $ setCredsRedirect $ tokenCreds token
        Nothing ->
            loginErrorMessageI LoginR Msg.InvalidLogin


tokenCreds :: YesodAuth master => Text -> Creds master
tokenCreds token = Creds "token" token []
