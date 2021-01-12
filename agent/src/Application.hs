{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Application
    ( appMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , getAppSettings
    , shutdownAll
    , shutdownWeb
    , startWeb
    -- * for GHCI
    , handler
    , runDb
    , getAgentCtx
    , sleep
    )
where

import           Startlude               hiding (runReader)

import           Control.Concurrent.STM.TVar    ( newTVarIO )
import           Control.Monad.Logger
import           Control.Effect.Labelled        ( Labelled, runLabelled )
import qualified Data.HashMap.Strict           as HM
import           Data.IORef

import           Database.Persist.Sql
import           Database.Persist.Sqlite        ( createSqlitePool
                                                , runSqlite
                                                , sqlPoolSize
                                                , sqlDatabase
                                                )
import           Git.Embed
import           Network.HTTP.Client.TLS        ( getGlobalManager )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( getPort )
import           System.Directory               ( createDirectoryIfMissing )
import           System.Environment             ( setEnv )
import           System.IO               hiding ( putStrLn, writeFile )
import           System.Log.FastLogger          ( defaultBufSize
                                                , newStdoutLoggerSet
                                                )
import           Yesod.Core
import           Yesod.Default.Config2
import           Yesod.Persist.Core

import           Constants
import qualified Daemon.AppNotifications       as AppNotifications
import           Daemon.RefreshProcDev
import           Daemon.ZeroConf
import           Foundation
import           Lib.Algebra.State.RegistryUrl
import           Lib.Database
import           Lib.External.Metrics.ProcDev
import           Lib.SelfUpdate
import           Lib.Sound
import           Lib.SystemPaths
import           Lib.WebServer
import           Model
import           Settings
import Lib.Background
import qualified Daemon.SslRenew as SSLRenew
import Lib.Tor (newTorManager)
import Daemon.TorHealth

appMain :: IO ()
appMain = do
    hSetBuffering stdout LineBuffering
    args <- getArgs

    -- Get the settings from all relevant sources
    settings <- loadYamlSettings [] [configSettingsYmlValue] useEnv

    settings' <- case args of
        ["--port", n] -> case readMaybe @Word16 $ toS n of
            Just n' -> pure $ settings { appPort = n' }
            Nothing -> do
                die . toS $ "Invalid Port: " <> n
        ["--git-hash"] -> do
            putStrLn @Text $embedGitRevision
            exitSuccess
        ["--version"] -> do
            putStrLn @Text (show agentVersion)
            exitSuccess
        _ -> pure settings
    createDirectoryIfMissing False (toS $ agentDataDirectory `relativeTo` appFilesystemBase settings')

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings'

    startupSequence foundation

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO AgentCtx
makeFoundation appSettings = do
    now                         <- getCurrentTime
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appLogger                   <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appHttpManager              <- getGlobalManager
    appTorManager               <- newTorManager (appTorSocksPort appSettings)
    appWebServerThreadId        <- newIORef Nothing
    appSelfUpdateSpecification  <- newEmptyMVar
    appIsUpdating               <- newIORef Nothing
    appIsUpdateFailed           <- newIORef Nothing
    appBackgroundJobs           <- newTVarIO (JobCache HM.empty)
    def                         <- getDefaultProcDevMetrics
    appProcDevMomentCache       <- newIORef (now, mempty, def)
    appLastTorRestart           <- newIORef now

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool appIconTags = AgentCtx { .. }
    -- The AgentCtx {..} syntax is an example of record wild cards. For more
    -- information, see:
    -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation
            (panic "connPool forced in tempFoundation")
            (panic "iconTags forced in tempFoundation")
        logFunc        = messageLoggerSource tempFoundation appLogger

    db <- interpDb dbPath

    -- Create the database connection pool, will create sqlite file if doesn't already exist
    pool <- flip runLoggingT logFunc $ createSqlitePool (toS db) (sqlPoolSize . appDatabaseConf $ appSettings)

    -- run migrations only if agent in charge
    when (appPort appSettings == 5959) $ do
        runSqlite db $ runMigration migrateAll
        void . interpDb $ ensureCoherentDbVersion pool logFunc

    iconTags <- if appPort appSettings == 5959
        then do
            iconDigests <- runSqlPool (selectList [] []) pool
            newTVarIO . HM.fromList $ (unIconDigestKey . entityKey &&& iconDigestTag . entityVal) <$> iconDigests
        else newTVarIO HM.empty

    -- Return the foundation
    pure $ mkFoundation pool iconTags
    where
        interpDb :: (Labelled "sqlDatabase" (ReaderT Text)) (Labelled "filesystemBase" (ReaderT Text) IO) a -> IO a
        interpDb = injectFilesystemBaseFromContext appSettings
                . flip runReaderT (sqlDatabase . appDatabaseConf $ appSettings)
                . runLabelled @"sqlDatabase"

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv


startupSequence :: AgentCtx -> IO ()
startupSequence foundation = do

#ifdef DISABLE_AUTH
    withAgentVersionLog_ "[WARNING] Agent auth disabled!"
#endif

    injectFilesystemBaseFromContext (appSettings foundation) . runRegistryUrlIOC $ getRegistryUrl >>= \case
        Nothing -> pure ()
        Just x -> liftIO $ do
            withAgentVersionLog "Detected Alternate Registry URL" x
            -- this is so that appmgr inherits the alternate registry url when it is called.
            setEnv "REGISTRY_URL" (show x)

    -- proc dev metrics refresh loop
    withAgentVersionLog_ "Initializing proc dev refresh loop"
    void . forkIO . forever $ forkIO (refreshProcDev foundation) >> threadDelay 5_000_000
    withAgentVersionLog_ "Proc dev metrics refreshing"

    -- web
    withAgentVersionLog_ "Starting web server"
    void . forkIO . startWeb $ foundation
    withAgentVersionLog_ "Web server running"

    -- all these actions are destructive in some way, and only webserver is needed for self-update
    when (appPort (appSettings foundation) == 5959) $ do
        synchronizeSystemState foundation agentVersion

        -- app notifications refresh loop
        withAgentVersionLog_ "Initializing app notifications refresh loop"
        void . forkIO . forever $ forkIO (runReaderT AppNotifications.fetchAndSave foundation) >> threadDelay 5_000_000
        withAgentVersionLog_ "App notifications refreshing"

        withAgentVersionLog_ "Initializing SSL certificate renewal loop"
        void . forkIO . forever $ forkIO (SSLRenew.renewSslLeafCert foundation) *> sleep 86_400
        withAgentVersionLog_ "SSL Renewal daemon started"

        withAgentVersionLog_ "Initializing Tor health check loop"
        void . forkIO . forever $ forkIO (runReaderT torHealth foundation) *> sleep 300
        withAgentVersionLog_ "Tor health check loop running"

        -- reloading avahi daemon
        -- DRAGONS! make sure this step happens AFTER system synchronization
        withAgentVersionLog_ "Publishing Agent to Avahi Daemon"
        runReaderT publishAgentToAvahi foundation
        withAgentVersionLog_ "Avahi Daemon reloaded with Agent service"

        when (appPort (appSettings foundation) == 5959) $ do
            playSong 400 marioCoin

    withAgentVersionLog_ "Listening for Self-Update Signal"
    waitForUpdateSignal foundation

sleep :: Integer -> IO ()
sleep n = let (full, r) = (n * 1_000_000) `divMod` fromIntegral (maxBound :: Int) in
    replicateM_ (fromIntegral full) (threadDelay maxBound) *> threadDelay (fromIntegral r)

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the AgentCtx from GHCi)
--------------------------------------------------------------

getApplicationRepl :: IO (Int, AgentCtx, Application)
getApplicationRepl = do
    foundation <- getAppSettings >>= makeFoundation
    wsettings  <- getDevSettings $ warpSettings foundation
    app1       <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

getAgentCtx :: IO AgentCtx
getAgentCtx = getAppSettings >>= makeFoundation

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
runDb :: ReaderT SqlBackend Handler a -> IO a
runDb = handler . runDB

