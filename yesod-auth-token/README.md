# Instructions

clone this repo under in Yesod project directory. Add the cloned repo in your cabal sandbox:

`cabal sandbox add-source yesod-auth-token`

# Example

To put in your `Foundation.hs`

```haskell
import Yesod.Auth.Token

instance YesodAuthToken App where
    type AuthTokenId App = UserId

    setUserToken uid t = do
        runDB $ updateWhere [UserId ==. uid] [UserToken =. t]

    getTokenCreds t = do
        mUser <- runDB . getBy $ UniqueToken t
        return $ (\uid -> TokenCreds (entityKey uid) t) <$> mUser

-- stuff in between

instance YesodAuth App where
    -- stuff
    getAuthId (Creds "token" t _) = do
        mTokenCreds <- getTokenCreds t
        return $ tokenCredsAuthId <$> mTokenCreds

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authToken]
```

In your model, you can have this

```
User
    token Text
    nickname Text Maybe
      UniqueToken token
      deriving Typeable
```
