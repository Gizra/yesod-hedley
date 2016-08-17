module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.OAuth2.Github
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appServerEvent :: (Chan ServerEvent)
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        (title, parents) <- breadcrumbs
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_semantic_min_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Nothing

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized HomeR _ = return Authorized
    isAuthorized CommentR _ = return Authorized
    isAuthorized (CompanyR _) _ = return Authorized

    isAuthorized AddMembershipR _ = isAuthenticated
    isAuthorized (EventR _) _ = isAuthenticated
    isAuthorized EventsR _ = isAuthenticated
    isAuthorized MyAccountR _ = isAuthenticated
    isAuthorized PeopleR _ = isAuthenticated
    isAuthorized (UserR _) _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger


isAuthenticated = do
    mu <- maybeAuthId
    return $ case mu of
        Nothing -> Unauthorized "You must login"
        Just _ -> Authorized

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $
        case credsPlugin creds of
            "github" -> do
                let ident = fromMaybe "" $ lookup "login" $ credsExtra creds
                x <- getBy . UniqueUser $ ident
                case x of
                    Just (Entity uid _) -> return $ Authenticated uid
                    Nothing -> Authenticated <$> insert User
                        { userIdent = ident
                        , userEmail = fromMaybe "" $ lookup "email" $ credsExtra creds
                        , userPassword = Nothing
                        , userVerkey = Nothing
                        }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins app = [ oauth2Github
                          (oauthKeysClientId     githubKeys)
                          (oauthKeysClientSecret githubKeys)
                      ]
                      where githubKeys = appGithubKeys $ appSettings app

    authHttpManager = getHttpManager

    maybeAuthId = do
        mToken <- lookupGetParam "access_token"
        tokenAuth <- case mToken of
            Nothing -> return Nothing
            Just token -> do
                mUser <- runDB $ selectFirst [AccessTokenToken ==. token] []
                case mUser of
                    Nothing -> return Nothing
                    Just user -> return $ Just . accessTokenUserId $ entityVal user
        defaultAuth <- defaultMaybeAuthId
        return $ case catMaybes [defaultAuth, tokenAuth] of
            [] -> Nothing
            (x : _) -> Just x

instance YesodBreadcrumbs App where
  breadcrumb HomeR      = return ("home", Nothing)
  breadcrumb AddMembershipR = return ("Membership", Just HomeR)
  breadcrumb MyAccountR = return ("My Account", Just HomeR)
  breadcrumb PeopleR = return ("People", Just HomeR)
  breadcrumb (CompanyR companyId) = do
    company <- runDB $ get404 companyId
    return (companyTitle company, Just HomeR)
  breadcrumb (UserR userId) = do
    user <- runDB $ get404 userId
    return (userIdent user ++ "'s account", Just PeopleR)
  breadcrumb  _ = return ("home", Nothing)


instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
