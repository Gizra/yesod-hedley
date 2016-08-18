{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.MySQL               (createMySQLPool, myConnInfo,
                                             myPoolSize, runSqlPool)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
import State

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home
import Handler.Comment
import Handler.Events
import Handler.Event
import Handler.People
import Handler.MyAccount
import Handler.User
import Handler.AddMembership
import Handler.Company
import Handler.SseReceive

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appServerEvent <- newChan

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createMySQLPool
        (myConnInfo $ appDatabaseConf appSettings)
        (myPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    _ <- migrateData pool

    -- Return the foundation
    return $ mkFoundation pool

migrateData pool = do
    -- Migrate data only if "admin" is missing.
    maybeUser <- runSqlPool (getBy $ UniqueUser "admin") pool
    case maybeUser of
        Just (Entity _ _) -> do
            putStrLn "---- Skipped migration"
            return ()

        Nothing -> do
            -- User
            userId1 <- runSqlPool (insert $ createUser "admin") pool
            userId2 <- runSqlPool (insert $ createUser "demo")  pool
            userId3 <- runSqlPool (insert $ createUser "migo")  pool

            -- Company
            company1 <- runSqlPool (insert $ Company "company1" userId1) pool
            company2 <- runSqlPool (insert $ Company "company2" userId2) pool
            company3 <- runSqlPool (insert $ Company "company3" userId2) pool

            -- Event
            _ <- runSqlPool (insert $ Event "post 1" "body 1" userId1) pool
            _ <- runSqlPool (insert $ Event "post 2" "body 2" userId2) pool
            _ <- runSqlPool (insert $ Event "post 3" "body 3" userId3) pool

            _ <- runSqlPool (insert $ Event "post 4" "body 4" userId1) pool
            _ <- runSqlPool (insert $ Event "post 5" "body 5" userId2) pool
            _ <- runSqlPool (insert $ Event "post 6" "body 6" userId3) pool

            currentTime <- getCurrentTime

            -- AccessToken
            _ <- runSqlPool (insert $ AccessToken currentTime userId1 "1234") pool
            _ <- runSqlPool (insert $ AccessToken currentTime userId2 "5678") pool

            -- Group membership
            _ <- runSqlPool (insert $ GroupMembership State.Active currentTime userId1 company1) pool
            _ <- runSqlPool (insert $ GroupMembership State.Active currentTime userId1 company2) pool
            _ <- runSqlPool (insert $ GroupMembership State.Pending currentTime userId1 company3) pool

            _ <- runSqlPool (insert $ GroupMembership State.Active currentTime userId2 company3) pool

            -- Don't return anything.
            return ()
            where
                createUser name =
                    User
                        { userIdent = name
                        , userEmail = name ++ "@example.com"
                        , userPassword = Nothing
                        , userVerkey = Nothing
                        }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
