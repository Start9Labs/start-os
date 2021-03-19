{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Backups where

import           Startlude               hiding ( Reader
                                                , ask
                                                , runReader
                                                )

import           Control.Carrier.Error.Church
import           Control.Carrier.Lift
import           Control.Carrier.Reader         ( runReader )
import           Control.Effect.Labelled hiding ( Handler )
import           Control.Effect.Reader.Labelled
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import           Data.UUID.V4
import           Database.Persist.Sql
import           Yesod.Auth
import           Yesod.Core
import           Yesod.Core.Types

import           Control.Concurrent.STM
import           Exinst
import           Foundation
import           Handler.Network
import           Handler.Util
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.Background
import           Lib.Error
import qualified Lib.External.AppMgr           as AppMgr
import qualified Lib.Notifications             as Notifications
import           Lib.Password
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model


data CreateBackupReq = CreateBackupReq
    { createBackupLogicalName :: FilePath
    , createBackupPassword    :: Maybe Text
    }
    deriving (Eq, Show)
instance FromJSON CreateBackupReq where
    parseJSON = withObject "Create Backup Req" $ \o -> do
        createBackupLogicalName <- o .: "logicalname"
        createBackupPassword    <- o .:? "password" .!= Nothing
        pure CreateBackupReq { .. }

data RestoreBackupReq = RestoreBackupReq
    { restoreBackupLogicalName :: FilePath
    , restoreBackupPassword    :: Maybe Text
    }
    deriving (Eq, Show)
instance FromJSON RestoreBackupReq where
    parseJSON = withObject "Restore Backup Req" $ \o -> do
        restoreBackupLogicalName <- o .: "logicalname"
        restoreBackupPassword    <- o .:? "password" .!= Nothing
        pure RestoreBackupReq { .. }

data EjectDiskReq = EjectDiskReq
    { ejectDiskLogicalName :: Text
    }
    deriving (Eq, Show)
instance FromJSON EjectDiskReq where
    parseJSON = withObject "Eject Disk Req" $ \o -> do
        ejectDiskLogicalName <- o .: "logicalName"
        pure EjectDiskReq { .. }

-- Handlers

postCreateBackupR :: AppId -> Handler ()
postCreateBackupR appId = disableEndpointOnFailedUpdate $ do
    req           <- requireCheckJsonBody
    AgentCtx {..} <- getYesod
    account       <- entityVal <$> requireAuth
    case validatePass account <$> (createBackupPassword req) of
        Just False -> runM . handleS9ErrC $ throwError BackupPassInvalidE
        _ ->
            createBackupLogic appId req
                & AppMgr2.runAppMgrCliC
                & runLabelled @"databaseConnection"
                & runReader appConnPool
                & runLabelled @"backgroundJobCache"
                & runReader appBackgroundJobs
                & handleS9ErrC
                & runM


postStopBackupR :: AppId -> Handler ()
postStopBackupR appId = disableEndpointOnFailedUpdate $ do
    cache <- getsYesod appBackgroundJobs
    stopBackupLogic appId & runLabelled @"backgroundJobCache" & runReader cache & handleS9ErrC & runM

postRestoreBackupR :: AppId -> Handler ()
postRestoreBackupR appId = disableEndpointOnFailedUpdate $ do
    req           <- requireCheckJsonBody
    AgentCtx {..} <- getYesod
    restoreBackupLogic appId req
        & AppMgr2.runAppMgrCliC
        & runLabelled @"databaseConnection"
        & runReader appConnPool
        & runLabelled @"backgroundJobCache"
        & runReader appBackgroundJobs
        & runLabelled @"lanThread"
        & runReader appLanThread
        & handleS9ErrC
        & runM

getDisksR :: Handler (JSONResponse [AppMgr.DiskInfo])
getDisksR = fmap JSONResponse . runM . handleS9ErrC $ listDisksLogic

postEjectR :: Handler ()
postEjectR = runM . handleS9ErrC $ requireCheckJsonBody >>= ejectDiskLogic . ejectDiskLogicalName

-- Logic

createBackupLogic :: ( HasLabelled "backgroundJobCache" (Reader (TVar JobCache)) sig m
                     , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
                     , Has (Error S9Error) sig m
                     , Has AppMgr2.AppMgr sig m
                     , MonadIO m
                     )
                  => AppId
                  -> CreateBackupReq
                  -> m ()
createBackupLogic appId CreateBackupReq {..} = do
    jobCache <- ask @"backgroundJobCache"
    db       <- ask @"databaseConnection"
    version  <- fmap AppMgr2.infoResVersion $ AppMgr2.info [AppMgr2.flags| |] appId `orThrowM` NotFoundE "appId"
                                                                                                         (show appId)
    res <- liftIO . atomically $ do
        (JobCache jobs) <- readTVar jobCache
        case HM.lookup appId jobs of
            Just (Some1 SCreatingBackup  _, _) -> pure (Left $ BackupE appId "Already creating backup")
            Just (Some1 SRestoringBackup _, _) -> pure (Left $ BackupE appId "Cannot backup during restore")
            Just (Some1 _                _, _) -> pure (Left $ BackupE appId "Cannot backup: incompatible status")
            Nothing                            -> do
                -- this panic is here because we don't have the threadID yet, and it is required. We want to write the
                -- TVar anyway though so that we don't accidentally launch multiple backup jobs
                -- TODO: consider switching to MVar's for this
                modifyTVar jobCache (insertJob appId Backup $ panic "ThreadID prematurely forced")
                pure $ Right ()
    case res of
        Left  e  -> throwError e
        Right () -> do
            tid <- liftIO . forkIO $ do
                appmgrRes <- runExceptT (AppMgr.backupCreate createBackupPassword appId createBackupLogicalName)
                atomically $ modifyTVar' jobCache (deleteJob appId)
                let notif = case appmgrRes of
                        Left  e -> Notifications.BackupFailed e
                        Right _ -> Notifications.BackupSucceeded
                flip runSqlPool db $ do
                    void $ insertBackupResult appId version (isRight appmgrRes)
                    void $ Notifications.emit appId version notif
            liftIO . atomically $ modifyTVar jobCache (insertJob appId Backup tid)

stopBackupLogic :: ( HasLabelled "backgroundJobCache" (Reader (TVar JobCache)) sig m
                   , Has (Error S9Error) sig m
                   , MonadIO m
                   )
                => AppId
                -> m ()
stopBackupLogic appId = do
    jobCache <- ask @"backgroundJobCache"
    res      <- liftIO . atomically $ do
        (JobCache jobs) <- readTVar jobCache
        case HM.lookup appId jobs of
            Just (Some1 SCreatingBackup _, tid) -> do
                modifyTVar jobCache (deleteJob appId)
                pure (Right tid)
            Just (Some1 SRestoringBackup _, _) -> pure (Left $ BackupE appId "Cannot interrupt restore")
            _ -> pure (Left $ NotFoundE "backup job" (show appId))
    case res of
        Left  e   -> throwError e
        Right tid -> liftIO $ killThread tid

restoreBackupLogic :: ( HasLabelled "backgroundJobCache" (Reader (TVar JobCache)) sig m
                      , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
                      , HasLabelled "lanThread" (Reader (MVar ThreadId)) sig m
                      , Has (Error S9Error) sig m
                      , Has AppMgr2.AppMgr sig m
                      , MonadIO m
                      )
                   => AppId
                   -> RestoreBackupReq
                   -> m ()
restoreBackupLogic appId RestoreBackupReq {..} = do
    lanThread <- ask @"lanThread"
    jobCache  <- ask @"backgroundJobCache"
    db        <- ask @"databaseConnection"
    version   <- fmap AppMgr2.infoResVersion $ AppMgr2.info [AppMgr2.flags| |] appId `orThrowM` NotFoundE "appId"
                                                                                                          (show appId)
    res <- liftIO . atomically $ do
        (JobCache jobs) <- readTVar jobCache
        case HM.lookup appId jobs of
            Just (Some1 SCreatingBackup  _, _) -> pure (Left $ BackupE appId "Cannot restore during backup")
            Just (Some1 SRestoringBackup _, _) -> pure (Left $ BackupE appId "Already restoring backup")
            Just (Some1 _                _, _) -> pure (Left $ BackupE appId "Cannot backup: incompatible status")
            Nothing                            -> do
                -- this panic is here because we don't have the threadID yet, and it is required. We want to write the
                -- TVar anyway though so that we don't accidentally launch multiple backup jobs
                -- TODO: consider switching to MVar's for this
                modifyTVar jobCache (insertJob appId Restore $ panic "ThreadID prematurely forced")
                pure $ Right ()
    case res of
        Left  e -> throwError e
        Right _ -> do
            tid <- liftIO . forkIO $ do
                appmgrRes <- runExceptT (AppMgr.backupRestore restoreBackupPassword appId restoreBackupLogicalName)
                atomically $ modifyTVar jobCache (deleteJob appId)
                let notif = case appmgrRes of
                        Left  e -> Notifications.RestoreFailed e
                        Right _ -> Notifications.RestoreSucceeded
                resetRes <- runExceptT @S9Error $ runReader lanThread . runLabelled @"lanThread" $ postResetLanLogic
                case resetRes of
                    Left  _  -> pure () -- temporarily forbidden is the only possible thing here so ignore it
                    Right () -> pure ()
                flip runSqlPool db $ void $ Notifications.emit appId version notif
            liftIO . atomically $ modifyTVar jobCache (insertJob appId Restore tid)

listDisksLogic :: (Has (Error S9Error) sig m, MonadIO m) => m [AppMgr.DiskInfo]
listDisksLogic = runExceptT AppMgr.diskShow >>= liftEither

ejectDiskLogic :: (Has (Error S9Error) sig m, MonadIO m) => Text -> m ()
ejectDiskLogic t = do
    (ec, _) <- AppMgr.readProcessInheritStderr "eject" [toS t] ""
    case ec of
        ExitSuccess   -> pure ()
        ExitFailure n -> throwError $ EjectE n

insertBackupResult :: MonadIO m => AppId -> Version -> Bool -> SqlPersistT m (Entity BackupRecord)
insertBackupResult appId appVersion succeeded = do
    uuid <- liftIO nextRandom
    now  <- liftIO getCurrentTime
    let k = (BackupRecordKey uuid)
    let v = (BackupRecord now appId appVersion succeeded)
    insertKey k v
    pure $ Entity k v

getLastSuccessfulBackup :: MonadIO m => AppId -> SqlPersistT m (Maybe UTCTime)
getLastSuccessfulBackup appId = backupRecordCreatedAt . entityVal <<$>> selectFirst
    [BackupRecordAppId ==. appId, BackupRecordSucceeded ==. True]
    [Desc BackupRecordCreatedAt]
