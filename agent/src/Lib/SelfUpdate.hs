{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Lib.SelfUpdate where

import           Startlude               hiding ( handle
                                                , runReader
                                                )

import           Control.Carrier.Error.Either
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8         as B8
import           Data.IORef
import           Data.List
import           Data.String.Interpolate.IsString
import           System.Posix.Files
import           System.Process

import           Constants
import           Database.Persist.Sqlite        ( runSqlPool )
import           Foundation
import           Handler.Types.V0.Base
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import           Lib.External.Registry
import qualified Lib.Notifications             as Notifications
import           Lib.Sound                     as Sound
import           Lib.Synchronizers
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.WebServer
import           Settings
import           UnliftIO.Exception             ( handle )

youngAgentPort :: Word16
youngAgentPort = 5960

waitForUpdateSignal :: AgentCtx -> IO ()
waitForUpdateSignal foundation = do
    eNewVersion <- runS9ErrT $ do
        spec <- lift . takeMVar . appSelfUpdateSpecification $ foundation
        let settings = appSettings foundation
        v <- interp settings (getLatestAgentVersionForSpec spec) >>= \case
            Nothing -> throwE $ UpdateSelfE GetLatestCompliantVersion "Not Found"
            Just v  -> pure v
        liftIO $ writeIORef (appIsUpdating foundation) (Just v)
        updateAgent foundation spec
    case eNewVersion of
        Right (newVersion, youngAgentProcess) -> do
            putStrLn @Text $ "New agent up and running: " <> show newVersion
            runReaderT replaceExecutableWithYoungAgent (appSettings foundation)
            killYoungAgent youngAgentProcess
            shutdownAll []
        Left e@(UpdateSelfE GetYoungAgentBinary _) -> do
            logerror e
            writeIORef (appIsUpdating foundation) Nothing
            waitForNextUpdateSignal
        Left e@(UpdateSelfE ShutdownWeb _) -> do
            logerror e
            writeIORef (appIsUpdating foundation) Nothing
            waitForNextUpdateSignal
        Left e@(UpdateSelfE StartupYoungAgent _) -> do
            logerror e
            writeIORef (appIsUpdating foundation) Nothing
            waitForNextUpdateSignal
        Left e@(UpdateSelfE (PingYoungAgent youngAgentProcess) _) -> do
            logerror e
            killYoungAgent youngAgentProcess
            writeIORef (appIsUpdating foundation) Nothing
            waitForNextUpdateSignal
        Left e -> do -- unreachable
            logerror e
            waitForNextUpdateSignal
    where
        waitForNextUpdateSignal = waitForUpdateSignal foundation
        logerror                = putStrLn @Text . show
        interp s = ExceptT . liftIO . runError . injectFilesystemBaseFromContext s . runRegistryUrlIOC


updateAgent :: AgentCtx -> VersionRange -> S9ErrT IO (Version, ProcessHandle)
updateAgent foundation avs = do
    -- get and save the binary of the new agent app
    putStrLn @Text $ "Acquiring young agent binary for specification: " <> show avs
    (tryTo . interp settings . getYoungAgentBinary $ avs) >>= \case
        Left  e -> throwE $ UpdateSelfE GetYoungAgentBinary (show e)
        Right _ -> putStrLn @Text "Succeeded"

    -- start the new agent app. This is non blocking as a success would block indefinitely
    startupYoungAgentProcessHandle <- startup 5

    putStrLn @Text $ "Beginning young agent ping attempts..."
    let attemptPing = do
            lift (threadDelay delayBetweenAttempts)
            tryTo pingYoungAgent >>= \case
                Left e -> do
                    putStrLn @Text (show e)
                    pure (Left e)
                x -> pure x
    retryAction attempts attemptPing >>= \case
        Left  e  -> throwE $ UpdateSelfE (PingYoungAgent startupYoungAgentProcessHandle) (show e)
        Right av -> putStrLn @Text "Succeeded" >> pure (av, startupYoungAgentProcessHandle)
    where
        tryTo                = lift . try @SomeException
        settings             = appSettings foundation
        attempts             = 8
        delayBetweenAttempts = 5 * 1000000 :: Int -- 5 seconds
        startup :: Int -> S9ErrT IO ProcessHandle
        startup startupAttempts = do
            putStrLn @Text $ "Starting up young agent..."
            tryTo (runReaderT startupYoungAgent $ appSettings foundation) >>= \case
                Left e -> if "busy" `isInfixOf` show e && startupAttempts > 0-- sometimes the file handle hasn't closed yet
                    then do
                        putStrLn @Text "agent-tmp busy, reattempting in 500ms"
                        liftIO (threadDelay 500_000)
                        startup (startupAttempts - 1)
                    else do
                        putStrLn @Text (show e)
                        throwE $ UpdateSelfE StartupYoungAgent (show e)
                Right ph -> putStrLn @Text "Succeeded" >> pure ph
        interp s = liftIO . injectFilesystemBaseFromContext s . injectFilesystemBaseFromContext s . runRegistryUrlIOC



retryAction :: Monad m => Integer -> m (Either e a) -> m (Either e a)
retryAction 1        action = action
retryAction maxTries action = do
    success <- action
    case success of
        Right a -> pure $ Right a
        Left  _ -> retryAction (maxTries - 1) action

replaceExecutableWithYoungAgent :: (MonadReader AppSettings m, MonadIO m) => m ()
replaceExecutableWithYoungAgent = do
    rt <- asks appFilesystemBase
    let tmpAgent = (executablePath `relativeTo` rt) </> tmpAgentFileName
    let agent    = (executablePath `relativeTo` rt) </> agentFileName

    liftIO $ removeLink (toS agent)
    liftIO $ rename (toS tmpAgent) (toS agent)


-- We assume that all app versions must listen on the same port.
youngAgentUrl :: Text
youngAgentUrl = "http://localhost:" <> show youngAgentPort

pingYoungAgent :: IO Version
pingYoungAgent = do
    (code, st_out, st_err) <- readProcessWithExitCode "curl" [toS $ toS youngAgentUrl </> "version"] ""
    putStrLn st_out
    putStrLn st_err
    case code of
        ExitSuccess -> case decodeStrict $ B8.pack st_out of
            Nothing                 -> throwIO . InternalS9Error $ "unparseable version: " <> toS st_out
            Just (AppVersionRes av) -> pure av
        ExitFailure e -> throwIO . InternalS9Error $ "curl failure with exit code: " <> show e

startupYoungAgent :: (MonadReader AppSettings m, MonadIO m) => m ProcessHandle
startupYoungAgent = do
    rt <- asks appFilesystemBase
    let cmd = (proc (toS $ (executablePath `relativeTo` rt) </> tmpAgentFileName) ["--port", show youngAgentPort])
            { create_group = True
            }
    ph <- liftIO $ view _4 <$> createProcess cmd
    liftIO $ threadDelay 1_000_000 -- 1 second
    liftIO $ getProcessExitCode ph >>= \case
        Nothing -> pure ph
        Just e  -> throwIO . InternalS9Error $ "young agent exited prematurely with exit code: " <> show e

killYoungAgent :: ProcessHandle -> IO ()
killYoungAgent p = do
    mEC <- getProcessExitCode p
    case mEC of
        Nothing -> interruptProcessGroupOf p
        Just _  -> pure ()
    threadDelay appEndEstimate
    where appEndEstimate = 10 * 1000000 :: Int --10 seconds

runSyncOps :: [SyncOp] -> ReaderT AgentCtx IO [(Bool, Bool)]
runSyncOps syncOps = do
    ctx <- ask
    let setUpdate b = if b
            then liftIO $ writeIORef (appIsUpdating ctx) (Just agentVersion)
            else liftIO $ writeIORef (appIsUpdating ctx) Nothing
    res <- for syncOps $ \syncOp -> do
        shouldRun <- syncOpShouldRun syncOp
        putStrLn @Text [i|Sync Op "#{syncOpName syncOp}" should run: #{shouldRun}|]
        when shouldRun $ do
            putStrLn @Text [i|Running Sync Op: #{syncOpName syncOp}|]
            setUpdate True
            syncOpRun syncOp
        pure $ (syncOpRequiresReboot syncOp, shouldRun)
    setUpdate False
    pure res

synchronizeSystemState :: AgentCtx -> Version -> IO ()
synchronizeSystemState ctx _version = handle @_ @SomeException cleanup $ flip runReaderT ctx $ do
    (restartsAndRuns, mTid) <- case synchronizer of
        Synchronizer { synchronizerOperations } -> flip runStateT Nothing $ for synchronizerOperations $ \syncOp -> do
            shouldRun <- lift $ syncOpShouldRun syncOp
            putStrLn @Text [i|Sync Op "#{syncOpName syncOp}" should run: #{shouldRun}|]
            when shouldRun $ do
                tid <- get >>= \case
                    Nothing -> do
                        tid <- liftIO . forkIO . forever $ playSong 300 updateInProgress *> threadDelay 20_000_000
                        put (Just tid)
                        pure tid
                    Just tid -> pure tid
                putStrLn @Text [i|Running Sync Op: #{syncOpName syncOp}|]
                setUpdate True
                lift $ handle @_ @SomeException (\e -> lift $ killThread tid *> cleanup e) $ syncOpRun syncOp
            pure $ (syncOpRequiresReboot syncOp, shouldRun)
    case mTid of
        Nothing  -> pure ()
        Just tid -> liftIO $ killThread tid
    setUpdate False
    when (any snd restartsAndRuns) $ liftIO $ do
        _ <- flip runSqlPool (appConnPool ctx)
            $ Notifications.emit (AppId "embassy-os") agentVersion Notifications.OsUpdateSucceeded
        playSong 400 marioPowerUp
    when (any (uncurry (&&)) restartsAndRuns) $ liftIO do
        callCommand "/bin/sync"
        callCommand "/sbin/reboot"
    where
        setUpdate :: MonadIO m => Bool -> m ()
        setUpdate b = if b
            then liftIO $ writeIORef (appIsUpdating ctx) (Just agentVersion)
            else liftIO $ writeIORef (appIsUpdating ctx) Nothing
        cleanup :: SomeException -> IO ()
        cleanup e = do
            void $ try @SomeException Sound.stop
            void $ try @SomeException Sound.unexport
            let e' = InternalE $ show e
            setUpdate False
            flip runReaderT ctx $ cantFail $ failUpdate e'

