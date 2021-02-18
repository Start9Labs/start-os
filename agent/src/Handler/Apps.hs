{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Apps where

import           Startlude               hiding ( modify
                                                , execState
                                                , asks
                                                , Reader
                                                , runReader
                                                , catchError
                                                , forkFinally
                                                , empty
                                                )

import           Control.Carrier.Reader
import           Control.Carrier.Error.Church
import           Control.Carrier.Lift
import qualified Control.Concurrent.Async.Lifted
                                               as LAsync
import qualified Control.Concurrent.Lifted     as Lifted
import qualified Control.Exception.Lifted      as Lifted
import           Control.Concurrent.STM.TVar
import           Control.Effect.Empty    hiding ( guard )
import           Control.Effect.Labelled        ( HasLabelled
                                                , Labelled
                                                , runLabelled
                                                )
import           Control.Lens            hiding ( (??) )
import           Control.Monad.Logger
import           Control.Monad.Trans.Control    ( MonadBaseControl )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy          as LBS
import           Data.IORef
import qualified Data.HashMap.Lazy             as HML
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import           Data.Singletons
import           Data.Singletons.Prelude.Bool   ( SBool(..)
                                                , If
                                                )
import           Data.Singletons.Prelude.List   ( Elem )

import           Database.Persist
import           Database.Persist.Sql           ( ConnectionPool )
import           Database.Persist.Sqlite        ( runSqlPool )
import           Exinst
import           Network.HTTP.Types
import           Yesod.Core.Content
import           Yesod.Core.Json
import           Yesod.Core.Handler      hiding ( cached )
import           Yesod.Core.Types               ( JSONResponse(..) )
import           Yesod.Persist.Core

import           Foundation
import           Handler.Backups
import           Handler.Icons
import           Handler.Types.Apps
import           Handler.Util
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.Algebra.State.RegistryUrl
import           Lib.Background
import           Lib.Error
import qualified Lib.External.AppMgr           as AppMgr
import qualified Lib.External.Registry         as Reg
import qualified Lib.External.AppManifest      as AppManifest
import           Lib.IconCache
import qualified Lib.Notifications             as Notifications
import           Lib.SystemPaths
import           Lib.TyFam.ConditionalData
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.ServerApp
import           Model
import           Settings
import           Crypto.Hash
import qualified Data.Text                     as Text
import           Lib.Types.NetAddress

pureLog :: Show a => a -> Handler a
pureLog = liftA2 (*>) ($logInfo . show) pure

logRet :: ToJSON a => Handler a -> Handler a
logRet = (>>= liftA2 (*>) ($logInfo . decodeUtf8 . LBS.toStrict . encode) pure)

mkAppStatus :: HM.HashMap AppId (BackupJobType, a) -> AppId -> AppContainerStatus -> AppStatus
mkAppStatus hm appId status = case HM.lookup appId hm of
    Nothing                 -> AppStatusAppMgr status
    Just (CreateBackup , _) -> AppStatusTmp CreatingBackup
    Just (RestoreBackup, _) -> AppStatusTmp RestoringBackup


type AllEffects m
    = AppMgr2.AppMgrCliC
          ( RegistryUrlIOC
                ( Labelled
                      "iconTagCache"
                      (ReaderT (TVar (HM.HashMap AppId (Digest MD5))))
                      ( Labelled
                            "filesystemBase"
                            (ReaderT Text)
                            ( Labelled
                                  "databaseConnection"
                                  (ReaderT ConnectionPool)
                                  (ReaderT AgentCtx (ErrorC S9Error (LiftC m)))
                            )
                      )
                )
          )

intoHandler :: AllEffects Handler x -> Handler x
intoHandler m = do
    ctx <- getYesod
    let fsbase = appFilesystemBase . appSettings $ ctx
    runM
        . handleS9ErrC
        . flip runReaderT ctx
        . flip runReaderT (appConnPool ctx)
        . runLabelled @"databaseConnection"
        . flip runReaderT fsbase
        . runLabelled @"filesystemBase"
        . flip runReaderT (appIconTags ctx)
        . runLabelled @"iconTagCache"
        . runRegistryUrlIOC
        . AppMgr2.runAppMgrCliC
        $ m
{-# INLINE intoHandler #-}

-- TODO nasty. Also, note that if AppMgr.getInstalledApp fails for any app we will not return available apps res.
getAvailableAppsR :: Handler (JSONResponse [AppAvailablePreview])
getAvailableAppsR = disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> getAvailableAppsLogic

getAvailableAppsLogic :: ( Has (Reader AgentCtx) sig m
                         , Has (Error S9Error) sig m
                         , Has RegistryUrl sig m
                         , Has AppMgr2.AppMgr sig m
                         , MonadIO m
                         , MonadBaseControl IO m
                         )
                      => m [AppAvailablePreview]
getAvailableAppsLogic = do
    jobCache <- asks appBackgroundJobs >>= liftIO . readTVarIO
    let installCache = inspect SInstalling jobCache
    (Reg.AppManifestRes apps, serverApps) <- LAsync.concurrently Reg.getAppManifest
                                                                 (AppMgr2.list [AppMgr2.flags|-s -d|])
    let remapped = remapAppMgrInfo jobCache serverApps
    pure $ foreach apps $ \app@StoreApp { storeAppId } ->
        let installing =
                (   (storeAppVersionInfoVersion . snd . installInfo &&& const (AppStatusTmp Installing))
                .   fst
                <$> HM.lookup storeAppId installCache
                )
            installed = ((view _2 &&& view _1) <$> HM.lookup storeAppId remapped)
        in  storeAppToAvailablePreview app $ installing <|> installed

getAvailableAppByIdR :: AppId -> Handler (JSONResponse AppAvailableFull)
getAvailableAppByIdR appId =
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> getAvailableAppByIdLogic appId

getAvailableAppByIdLogic :: ( Has (Reader AgentCtx) sig m
                            , Has (Error S9Error) sig m
                            , Has RegistryUrl sig m
                            , Has AppMgr2.AppMgr sig m
                            , MonadIO m
                            , MonadBaseControl IO m
                            )
                         => AppId
                         -> m AppAvailableFull
getAvailableAppByIdLogic appId = do
    let storeAppId' = storeAppId
    jobCache <- asks appBackgroundJobs >>= liftIO . readTVarIO
    let installCache = inspect SInstalling jobCache
    (Reg.AppManifestRes storeApps, serverApps) <- LAsync.concurrently Reg.getAppManifest
                                                                      (AppMgr2.list [AppMgr2.flags|-s -d|])
    StoreApp {..} <- pure (find ((== appId) . storeAppId) storeApps) `orThrowM` NotFoundE "appId" (show appId)
    let remapped = remapAppMgrInfo jobCache serverApps
    let installingInfo =
            (   (storeAppVersionInfoVersion . snd . installInfo &&& const (AppStatusTmp Installing))
                .   fst
                <$> HM.lookup appId installCache
                )
                <|> ((view _2 &&& view _1) <$> HM.lookup appId remapped)
    let latest = extract storeAppVersions
    dependencies <- AppMgr2.checkDependencies (AppMgr2.LocalOnly False)
                                              appId
                                              (Just . exactly $ storeAppVersionInfoVersion latest)
    enrichedDeps <- maybe (throwError (NotFoundE "dependencyId for" (show appId))) pure $ flip
        HML.traverseWithKey
        dependencies
        \depId depInfo ->
            let
                base = storeAppToAppBase <$> find ((== depId) . storeAppId') storeApps
                status =
                    (HM.lookup depId installCache $> AppStatusTmp Installing) <|> (view _1 <$> HM.lookup depId remapped)
            in
                (, status, depInfo) <$> base
    let dependencyRequirements = fmap (dependencyInfoToDependencyRequirement (AsInstalled SFalse)) enrichedDeps
    pure AppAvailableFull
        { appAvailableFullBase                   = AppBase
                                                       appId
                                                       storeAppTitle
                                                       (storeIconUrl appId (storeAppVersionInfoVersion $ extract storeAppVersions))
        , appAvailableFullInstallInfo            = installingInfo
        , appAvailableFullVersionLatest          = storeAppVersionInfoVersion latest
        , appAvailableFullDescriptionShort       = storeAppDescriptionShort
        , appAvailableFullDescriptionLong        = storeAppDescriptionLong
        , appAvailableFullReleaseNotes           = storeAppVersionInfoReleaseNotes latest
        , appAvailableFullDependencyRequirements = HM.elems dependencyRequirements
        , appAvailableFullVersions               = storeAppVersionInfoVersion <$> storeAppVersions
        , appAvailableFullInstallAlert           = storeAppVersionInfoInstallAlert latest
        }

getAppLogsByIdR :: AppId -> Handler (JSONResponse [Text])
getAppLogsByIdR appId = disableEndpointOnFailedUpdate $ handleS9ErrT $ do
    logs <- AppMgr.getAppLogs appId
    pure . JSONResponse . lines $ logs

getInstalledAppsR :: Handler (JSONResponse [AppInstalledPreview])
getInstalledAppsR = disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> getInstalledAppsLogic

cached :: MonadIO m => m a -> m (m a)
cached action = do
    ref <- liftIO $ newIORef Nothing
    pure $ liftIO (readIORef ref) >>= \case
        Nothing -> action >>= liftA2 (*>) (liftIO . writeIORef ref . Just) pure
        Just x  -> pure x

getInstalledAppsLogic :: (Has (Reader AgentCtx) sig m, Has AppMgr2.AppMgr sig m, MonadIO m) => m [AppInstalledPreview]
getInstalledAppsLogic = do
    jobCache <- asks appBackgroundJobs >>= liftIO . readTVarIO
    lanCache <- asks appLanThreads >>= liftIO . readTVarIO
    let installCache = installInfo . fst <$> inspect SInstalling jobCache
    serverApps <- AppMgr2.list [AppMgr2.flags|-s -d -m|]
    let remapped           = remapAppMgrInfo jobCache serverApps
        installingPreviews = flip
            HM.mapWithKey
            installCache
            \installingId (StoreApp {..}, StoreAppVersionInfo {..}) -> AppInstalledPreview
                { appInstalledPreviewBase             = AppBase installingId
                                                                storeAppTitle
                                                                (iconUrl installingId storeAppVersionInfoVersion)
                , appInstalledPreviewStatus           = AppStatusTmp Installing
                , appInstalledPreviewVersionInstalled = storeAppVersionInfoVersion
                , appInstalledPreviewTorAddress       = Nothing
                , appInstalledPreviewLanAddress       = Nothing
                , appInstalledPreviewUi               = False
                }
        installedPreviews = flip
            HML.mapWithKey
            remapped
            \appId (s, v, AppMgr2.InfoRes {..}) -> AppInstalledPreview
                { appInstalledPreviewBase             = AppBase appId infoResTitle (iconUrl appId v)
                , appInstalledPreviewStatus           = s
                , appInstalledPreviewVersionInstalled = v
                , appInstalledPreviewTorAddress       = infoResTorAddress
                , appInstalledPreviewLanAddress       = if appId `HM.member` lanCache
                                                            then
                                                                LanAddress
                                                                .   (".onion" `Text.replace` ".local")
                                                                .   unTorAddress
                                                                <$> infoResTorAddress
                                                            else Nothing
                , appInstalledPreviewUi               = AppManifest.uiAvailable infoResManifest
                }

    pure $ HML.elems $ HML.union installingPreviews installedPreviews

getInstalledAppByIdR :: AppId -> Handler (JSONResponse AppInstalledFull)
getInstalledAppByIdR appId =
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> getInstalledAppByIdLogic appId

getInstalledAppByIdLogic :: ( Has (Reader AgentCtx) sig m
                            , Has RegistryUrl sig m
                            , Has (Error S9Error) sig m
                            , Has AppMgr2.AppMgr sig m
                            , MonadIO m
                            , MonadBaseControl IO m
                            )
                         => AppId
                         -> m AppInstalledFull
getInstalledAppByIdLogic appId = do
    jobCache <- asks appBackgroundJobs >>= liftIO . readTVarIO
    let installCache = installInfo . fst <$> inspect SInstalling jobCache
    db          <- asks appConnPool
    backupTime' <- LAsync.async $ liftIO $ flip runSqlPool db $ getLastSuccessfulBackup appId
    let installing = do
            backupTime <- lift $ LAsync.wait backupTime'
            hoistMaybe $ HM.lookup appId installCache <&> \(StoreApp {..}, StoreAppVersionInfo {..}) -> AppInstalledFull
                { appInstalledFullBase = AppBase appId storeAppTitle (iconUrl appId storeAppVersionInfoVersion)
                , appInstalledFullStatus                 = AppStatusTmp Installing
                , appInstalledFullVersionInstalled       = storeAppVersionInfoVersion
                , appInstalledFullInstructions           = Nothing
                , appInstalledFullLastBackup             = backupTime
                , appInstalledFullTorAddress             = Nothing
                , appInstalledFullLanAddress             = Nothing
                , appInstalledFullConfiguredRequirements = []
                , appInstalledFullUninstallAlert         = Nothing
                , appInstalledFullRestoreAlert           = Nothing
                }
    serverApps <- AppMgr2.list [AppMgr2.flags|-s -d|]
    let remapped = remapAppMgrInfo jobCache serverApps
    appManifestFetchCached <- cached Reg.getAppManifest
    let
        installed = do
            (status, version, AppMgr2.InfoRes {..}) <- hoistMaybe (HM.lookup appId remapped)
            manifest' <- lift $ LAsync.async $ AppMgr2.infoResManifest <<$>> AppMgr2.info [AppMgr2.flags|-M|] appId
            instructions'                           <- lift $ LAsync.async $ AppMgr2.instructions appId
            requirements                            <- LAsync.runConcurrently $ flip
                HML.traverseWithKey
                (HML.filter AppMgr2.dependencyInfoRequired infoResDependencies)
                \depId depInfo -> LAsync.Concurrently $ do
                    let
                        fromInstalled = (AppMgr2.infoResTitle &&& AppMgr2.infoResVersion)
                            <$> hoistMaybe (HM.lookup depId serverApps)
                    let fromStore = do
                            Reg.AppManifestRes res <- lift appManifestFetchCached
                            (storeAppTitle &&& storeAppVersionInfoVersion . extract . storeAppVersions)
                                <$> hoistMaybe (find ((== depId) . storeAppId) res)
                    (title, v) <- fromInstalled <|> fromStore
                    let base = AppBase depId title (iconUrl depId v)
                    let
                        depStatus =
                            (HM.lookup depId installCache $> AppStatusTmp Installing)
                                <|> (view _1 <$> HM.lookup depId remapped)
                    pure $ dependencyInfoToDependencyRequirement (AsInstalled STrue) (base, depStatus, depInfo)
            manifest     <- lift $ LAsync.wait manifest'
            instructions <- lift $ LAsync.wait instructions'
            backupTime   <- lift $ LAsync.wait backupTime'
            lans         <- asks appLanThreads
            lanEnabled   <- liftIO $ HM.member appId <$> readTVarIO lans
            let lanAddress = if lanEnabled
                    then LanAddress . (".onion" `Text.replace` ".local") . unTorAddress <$> infoResTorAddress
                    else Nothing
            pure AppInstalledFull { appInstalledFullBase = AppBase appId infoResTitle (iconUrl appId version)
                                  , appInstalledFullStatus                 = status
                                  , appInstalledFullVersionInstalled       = version
                                  , appInstalledFullInstructions           = instructions
                                  , appInstalledFullLastBackup             = backupTime
                                  , appInstalledFullTorAddress             = infoResTorAddress
                                  , appInstalledFullLanAddress             = lanAddress
                                  , appInstalledFullConfiguredRequirements = HM.elems requirements
                                  , appInstalledFullUninstallAlert = manifest >>= AppManifest.appManifestUninstallAlert
                                  , appInstalledFullRestoreAlert = manifest >>= AppManifest.appManifestRestoreAlert
                                  }
    runMaybeT (installing <|> installed) `orThrowM` NotFoundE "appId" (show appId)

postUninstallAppR :: AppId -> Handler (JSONResponse (WithBreakages ()))
postUninstallAppR appId = do
    dry <- AppMgr2.DryRun . isJust <$> lookupGetParam "dryrun"
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> postUninstallAppLogic appId dry

postUninstallAppLogic :: ( HasFilesystemBase sig m
                         , Has (Reader AgentCtx) sig m
                         , Has (Error S9Error) sig m
                         , Has AppMgr2.AppMgr sig m
                         , MonadIO m
                         , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
                         , HasLabelled "iconTagCache" (Reader (TVar (HM.HashMap AppId (Digest MD5)))) sig m
                         )
                      => AppId
                      -> AppMgr2.DryRun
                      -> m (WithBreakages ())
postUninstallAppLogic appId dryrun = do
    jobCache <- asks appBackgroundJobs >>= liftIO . readTVarIO
    let tmpStatuses = statuses jobCache
    serverApps <- AppMgr2.list [AppMgr2.flags| |]
    when (not $ HM.member appId serverApps) $ throwError (AppNotInstalledE appId)
    case HM.lookup appId tmpStatuses of
        Just Installing      -> throwError (TemporarilyForbiddenE appId "uninstall" (show Installing))
        Just CreatingBackup  -> throwError (TemporarilyForbiddenE appId "uninstall" (show CreatingBackup))
        Just RestoringBackup -> throwError (TemporarilyForbiddenE appId "uninstall" (show RestoringBackup))
        _                    -> pure ()
    let flags = if coerce dryrun then Left dryrun else Right (AppMgr2.Purge True)
    breakageIds <- HM.keys . AppMgr2.unBreakageMap <$> AppMgr2.remove flags appId
    bs <- pure (traverse (hydrate $ (AppMgr2.infoResTitle &&& AppMgr2.infoResVersion) <$> serverApps) breakageIds)
        `orThrowM` InternalE "Reported app breakage for app that isn't installed, contact support"
    when (not $ coerce dryrun) $ clearIcon appId
    pure $ WithBreakages bs ()

type InstallResponse :: Bool -> Type
data InstallResponse a = InstallResponse (If a (WithBreakages ()) AppInstalledFull)
instance ToJSON (Some1 InstallResponse) where
    toJSON (Some1 STrue  (InstallResponse a)) = toJSON a
    toJSON (Some1 SFalse (InstallResponse a)) = toJSON a
postInstallNewAppR :: AppId -> Handler (JSONResponse (Some1 InstallResponse))
postInstallNewAppR appId = do
    dryrun <- isJust <$> lookupGetParam "dryrun"
    InstallNewAppReq { installNewAppVersion } <- requireCheckJsonBody
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> do
        withSomeSing dryrun $ \sb -> Some1 sb . InstallResponse <$> postInstallNewAppLogic appId installNewAppVersion sb

postInstallNewAppLogic :: forall sig m a
                        . ( Has (Reader AgentCtx) sig m
                          , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
                          , HasLabelled "iconTagCache" (Reader (TVar (HM.HashMap AppId (Digest MD5)))) sig m
                          , Has (Error S9Error) sig m
                          , Has RegistryUrl sig m
                          , Has AppMgr2.AppMgr sig m
                          , HasFilesystemBase sig m
                          , MonadIO m
                          , MonadBaseControl IO m
                          )
                       => AppId
                       -> Version
                       -> SBool a
                       -> m (If a (WithBreakages ()) AppInstalledFull)
postInstallNewAppLogic appId appVersion dryrun = do
    db   <- asks appConnPool
    full <- (Just <$> getInstalledAppByIdLogic appId) `catchError` \case
        NotFoundE "appId" appId' ->
            if AppId appId' == appId then pure Nothing else throwError (NotFoundE "appId" appId')
        other -> throwError other
    case full of
        Just aif@AppInstalledFull{} -> if appInstalledFullVersionInstalled aif == appVersion
            then pure $ case dryrun of
                STrue  -> WithBreakages [] ()
                SFalse -> aif
            else installIt db True
        Nothing -> installIt db False
    where
        installIt :: ConnectionPool -> Bool -> m (If a (WithBreakages ()) AppInstalledFull)
        installIt db isUpdate = do
            jobCacheTVar                <- asks appBackgroundJobs
            store@StoreApp {..}         <- Reg.getStoreAppInfo appId `orThrowM` NotFoundE "appId" (show appId)
            vinfo@StoreAppVersionInfo{} <-
                find ((== appVersion) . storeAppVersionInfoVersion) storeAppVersions
                    `orThrowPure` NotFoundE "version" (show appVersion)
            -- if it is a dry run of an update we don't want to modify the cache
            case dryrun of
                STrue -> if not isUpdate
                    then pure $ WithBreakages [] ()
                    else do
                        serverApps' <- LAsync.async $ AppMgr2.list [AppMgr2.flags| |]
                        hm          <- AppMgr2.update (AppMgr2.DryRun True) appId (Just $ exactly appVersion)
                        (serverApps :: HM.HashMap AppId (AppMgr2.InfoRes ( 'Right '[]))) <- LAsync.wait serverApps'
                        breakages   <-
                            traverse (hydrate ((AppMgr2.infoResTitle &&& AppMgr2.infoResVersion) <$> serverApps))
                                     (HM.keys $ AppMgr2.unBreakageMap hm)
                                `orThrowPure` InternalE
                                                  "Breakage reported for app that isn't installed, contact support"
                        pure $ WithBreakages breakages ()
                SFalse -> do
                    let
                        action = do
                            iconAction <- LAsync.async $ saveIcon (toS storeAppIconUrl)
                            let install = if isUpdate
                                    then void $ AppMgr2.update (AppMgr2.DryRun False) appId (Just $ exactly appVersion)
                                    else AppMgr2.install (AppMgr2.NoCache True) appId (Just $ exactly appVersion)
                            let
                                success = liftIO $ void $ flip runSqlPool db $ Notifications.emit
                                    appId
                                    appVersion
                                    Notifications.InstallSuccess
                            let failure e = liftIO $ do
                                    let notif = case e of
                                            AppMgrE _ ec -> Notifications.InstallFailedAppMgrExitCode ec
                                            _            -> Notifications.InstallFailedS9Error e
                                    void $ flip runSqlPool db $ Notifications.emit appId appVersion notif
                                    putStrLn @Text (show e)
                            let todo = do
                                    install
                                    () <- LAsync.wait iconAction
                                    success
                            todo `catchError` failure
                    tid <- action `Lifted.forkFinally` const postInstall
                    liftIO $ atomically $ modifyTVar' jobCacheTVar (insertJob appId (Install store vinfo) tid)
                    getInstalledAppByIdLogic appId
        postInstall :: m ()
        postInstall = do
            jobCache <- asks appBackgroundJobs
            pool     <- asks appConnPool
            liftIO . atomically $ modifyTVar jobCache (deleteJob appId)
            ls <- AppMgr2.list [AppMgr2.flags| |]
            LAsync.forConcurrently_ (HM.toList ls) $ \(k, AppMgr2.InfoRes {..}) -> when
                infoResNeedsRestart
                (            postRestartServerAppLogic k
                `catchError` \e -> liftIO $ runSqlPool
                                 (void $ Notifications.emit k infoResVersion (Notifications.RestartFailed e))
                                 pool
                )


postStartServerAppR :: AppId -> Handler ()
postStartServerAppR appId = disableEndpointOnFailedUpdate . intoHandler $ postStartServerAppLogic appId

postStartServerAppLogic :: (Has (Error S9Error) sig m, Has AppMgr2.AppMgr sig m, Has (Reader AgentCtx) sig m, MonadIO m)
                        => AppId
                        -> m ()
postStartServerAppLogic appId = do
    jobCache       <- asks appBackgroundJobs >>= liftIO . readTVarIO
    info           <- AppMgr2.info [AppMgr2.flags|-s -d|] appId `orThrowM` AppNotInstalledE appId
    (status, _, _) <- (HM.lookup appId $ remapAppMgrInfo jobCache (HM.singleton appId info))
        `orThrowPure` InternalE "Remapping magically deleted keys between source and target structures"
    case status of
        AppStatusAppMgr Stopped -> AppMgr2.start appId
        other                   -> throwError $ AppStateActionIncompatibleE appId other Start

postRestartServerAppR :: AppId -> Handler ()
postRestartServerAppR appId = disableEndpointOnFailedUpdate . intoHandler $ postRestartServerAppLogic appId

postRestartServerAppLogic :: ( Has (Reader AgentCtx) sig m
                             , Has AppMgr2.AppMgr sig m
                             , Has (Error S9Error) sig m
                             , MonadBaseControl IO m
                             , MonadIO m
                             )
                          => AppId
                          -> m ()
postRestartServerAppLogic appId = do
    jobCache <- asks appBackgroundJobs
    answer   <- Lifted.newEmptyMVar
    void . Lifted.fork $ do
        tid     <- Lifted.myThreadId
        problem <- liftIO . atomically $ do
            JobCache jobs <- readTVar jobCache
            case HM.lookup appId jobs of
                Just (Some1 s _, _) -> pure (Just . throwError $ TemporarilyForbiddenE appId "restart" (show s))
                Nothing             -> do
                    modifyTVar jobCache (insertJob appId RestartApp tid)
                    pure Nothing
        case problem of
            Nothing -> do
                AppMgr2.restart appId `Lifted.finally` (liftIO . atomically) (modifyTVar jobCache (deleteJob appId))
                Lifted.putMVar answer Nothing
            Just p -> Lifted.putMVar answer (Just p)
    Lifted.takeMVar answer >>= \case
        Nothing -> pure ()
        Just p  -> p


postStopServerAppR :: AppId -> Handler (JSONResponse (WithBreakages ()))
postStopServerAppR appId = disableEndpointOnFailedUpdate do
    dryrun <- isJust <$> lookupGetParam "dryrun"
    mRes   <- intoHandler $ runMaybeT (JSONResponse <$> postStopServerAppLogic appId (AppMgr2.DryRun dryrun))
    case mRes of
        Nothing -> sendResponseStatus status200 ()
        Just x  -> pure x

postStopServerAppLogic :: ( Has Empty sig m
                          , Has (Reader AgentCtx) sig m
                          , Has (Error S9Error) sig m
                          , Has AppMgr2.AppMgr sig m
                          , MonadIO m
                          , MonadBaseControl IO m
                          )
                       => AppId
                       -> AppMgr2.DryRun
                       -> m (WithBreakages ())
postStopServerAppLogic appId dryrun = do
    jobCache <- asks appBackgroundJobs
    titles   <- (AppMgr2.infoResTitle &&& AppMgr2.infoResVersion) <<$>> AppMgr2.list [AppMgr2.flags| |]
    let stopIt = do
            breakages <- AppMgr2.stop dryrun appId
            bases     <- traverse (hydrate titles) (HM.keys $ AppMgr2.unBreakageMap breakages)
                `orThrowPure` InternalE "Breakages reported for app that isn't installed, contact support"
            pure $ WithBreakages bases ()
    status <- AppMgr2.infoResStatus <<$>> AppMgr2.info [AppMgr2.flags|-S|] appId
    case (dryrun, status) of
        (_                   , Nothing     ) -> throwError $ NotFoundE "appId" (show appId)
        (AppMgr2.DryRun False, Just Running) -> do
            tid <- (void stopIt)
                `Lifted.forkFinally` const ((liftIO . atomically) (modifyTVar jobCache (deleteJob appId)))
            liftIO . atomically $ modifyTVar jobCache (insertJob appId StopApp tid)
            empty
        (AppMgr2.DryRun True , Just Running   ) -> stopIt
        (AppMgr2.DryRun False, Just Restarting) -> do
            tid <- (void stopIt)
                `Lifted.forkFinally` const ((liftIO . atomically) (modifyTVar jobCache (deleteJob appId)))
            liftIO . atomically $ modifyTVar jobCache (insertJob appId StopApp tid)
            empty
        (AppMgr2.DryRun True, Just Restarting) -> stopIt
        (_, Just other) -> throwError $ AppStateActionIncompatibleE appId (AppStatusAppMgr other) Stop

getAppConfigR :: AppId -> Handler TypedContent
getAppConfigR =
    disableEndpointOnFailedUpdate
        . handleS9ErrT
        . fmap (TypedContent typeJson . toContent)
        . AppMgr.getConfigurationAndSpec

patchAppConfigR :: AppId -> Handler (JSONResponse (WithBreakages ()))
patchAppConfigR appId = disableEndpointOnFailedUpdate $ do
    dryrun  <- isJust <$> lookupGetParam "dryrun"
    value   <- requireCheckJsonBody @_ @Value
    realVal <-
        runM . handleS9ErrC $ ((value ^? key "config") `orThrowPure` (InvalidRequestE value "Missing 'config' key"))
    intoHandler $ JSONResponse <$> patchAppConfigLogic appId (AppMgr2.DryRun dryrun) realVal

patchAppConfigLogic :: ( Has (Reader AgentCtx) sig m
                       , Has (Error S9Error) sig m
                       , Has AppMgr2.AppMgr sig m
                       , MonadBaseControl IO m
                       , MonadIO m
                       )
                    => AppId
                    -> AppMgr2.DryRun
                    -> Value
                    -> m (WithBreakages ())
patchAppConfigLogic appId dryrun cfg = do
    serverApps                <- AppMgr2.list [AppMgr2.flags| |]
    AppMgr2.ConfigureRes {..} <- AppMgr2.configure dryrun appId (Just cfg)
    when (not $ coerce dryrun) $ for_ configureResNeedsRestart postRestartServerAppLogic
    breakages <-
        traverse (hydrate ((AppMgr2.infoResTitle &&& AppMgr2.infoResVersion) <$> serverApps))
                 (HM.keys configureResStopped)
            `orThrowPure` InternalE "Breakage reported for app that is not installed, contact support"
    pure $ WithBreakages breakages ()


getAppNotificationsR :: AppId -> Handler (JSONResponse [Entity Notification])
getAppNotificationsR appId = disableEndpointOnFailedUpdate $ runDB $ do
    page     <- lookupGetParam "page" `orDefaultTo` 1
    pageSize <- lookupGetParam "perPage" `orDefaultTo` 20
    evs      <- selectList [NotificationAppId ==. appId]
                           [Desc NotificationCreatedAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
    let toArchive = fmap entityKey $ filter ((== Nothing) . notificationArchivedAt . entityVal) evs
    void $ Notifications.archive toArchive
    pure $ JSONResponse evs
    where
        orDefaultTo :: (Monad m, Read a) => m (Maybe Text) -> a -> m a
        orDefaultTo m a = do
            m' <- m
            case m' >>= readMaybe . toS of
                Nothing -> pure a
                Just x  -> pure x

getAppMetricsR :: AppId -> Handler TypedContent
getAppMetricsR appId =
    disableEndpointOnFailedUpdate . handleS9ErrT $ fmap (TypedContent typeJson . toContent) $ AppMgr.stats appId

getAvailableAppVersionInfoR :: AppId -> VersionRange -> Handler (JSONResponse AppVersionInfo)
getAvailableAppVersionInfoR appId version =
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> getAvailableAppVersionInfoLogic appId version

getAvailableAppVersionInfoLogic :: ( Has (Reader AgentCtx) sig m
                                   , Has (Error S9Error) sig m
                                   , Has RegistryUrl sig m
                                   , Has AppMgr2.AppMgr sig m
                                   , MonadIO m
                                   , MonadBaseControl IO m
                                   )
                                => AppId
                                -> VersionRange
                                -> m AppVersionInfo
getAvailableAppVersionInfoLogic appId appVersionSpec = do
    jobCache                     <- asks appBackgroundJobs >>= liftIO . readTVarIO
    Reg.AppManifestRes storeApps <- Reg.getAppManifest
    let titles =
            (storeAppTitle &&& storeAppVersionInfoVersion . extract . storeAppVersions) <$> indexBy storeAppId storeApps
    StoreApp {..} <- find ((== appId) . storeAppId) storeApps `orThrowPure` NotFoundE "appId" (show appId)
    serverApps    <- AppMgr2.list [AppMgr2.flags|-s -d|]
    let remapped = remapAppMgrInfo jobCache serverApps
    StoreAppVersionInfo {..} <-
        maximumMay (NE.filter ((<|| appVersionSpec) . storeAppVersionInfoVersion) storeAppVersions)
            `orThrowPure` NotFoundE "version spec " (show appVersionSpec)
    dependencies <- AppMgr2.checkDependencies (AppMgr2.LocalOnly False)
                                              appId
                                              (Just $ exactly storeAppVersionInfoVersion)
    requirements <- flip HML.traverseWithKey dependencies $ \depId depInfo -> do
        base <- hydrate titles depId `orThrowPure` NotFoundE "metadata for" (show depId)
        let status =
                (HM.lookup depId (inspect SInstalling jobCache) $> AppStatusTmp Installing)
                    <|> (view _1 <$> HM.lookup depId remapped)
        pure $ dependencyInfoToDependencyRequirement (AsInstalled SFalse) (base, status, depInfo)
    pure AppVersionInfo { appVersionInfoVersion                = storeAppVersionInfoVersion
                        , appVersionInfoReleaseNotes           = storeAppVersionInfoReleaseNotes
                        , appVersionInfoDependencyRequirements = HM.elems requirements
                        , appVersionInfoInstallAlert           = storeAppVersionInfoInstallAlert
                        }

postAutoconfigureR :: AppId -> AppId -> Handler (JSONResponse (WithBreakages AutoconfigureChangesRes))
postAutoconfigureR dependency dependent = do
    dry <- AppMgr2.DryRun . isJust <$> lookupGetParam "dryrun"
    disableEndpointOnFailedUpdate . intoHandler $ JSONResponse <$> postAutoconfigureLogic dependency dependent dry

postAutoconfigureLogic :: ( Has (Reader AgentCtx) sig m
                          , Has AppMgr2.AppMgr sig m
                          , Has (Error S9Error) sig m
                          , MonadBaseControl IO m
                          , MonadIO m
                          )
                       => AppId
                       -> AppId
                       -> AppMgr2.DryRun
                       -> m (WithBreakages AutoconfigureChangesRes)
postAutoconfigureLogic dependency dependent dry = do
    -- IMPORTANT! AppMgr reverses arguments from the endpoint
    appData <- AppMgr2.list [AppMgr2.flags| |]
    let apps = HM.keys appData
    case (dependency `elem` apps, dependent `elem` apps) of
        (False, _    ) -> throwError $ NotFoundE "appId" (show dependency)
        (_    , False) -> throwError $ NotFoundE "appId" (show dependent)
        _              -> pure ()
    AppMgr2.AutoconfigureRes {..} <- AppMgr2.autoconfigure dry dependent dependency
    when (not $ coerce dry) $ for_ (AppMgr2.configureResNeedsRestart autoconfigureConfigRes) postRestartServerAppLogic
    let titles = (AppMgr2.infoResTitle &&& AppMgr2.infoResVersion) <$> appData
    bases <- traverse (hydrate titles) (HM.keys (AppMgr2.configureResStopped autoconfigureConfigRes))
        `orThrowPure` InternalE "Breakages reported for app that isn't installed, contact support"
    pure $ WithBreakages bases (AutoconfigureChangesRes $ HM.lookup dependency autoconfigureChanged)

indexBy :: (Eq k, Hashable k) => (v -> k) -> [v] -> HM.HashMap k v
indexBy = flip foldr HM.empty . (>>= HM.insertWith const)
{-# INLINE indexBy #-}

hydrate :: HM.HashMap AppId (Text, Version) -> AppId -> Maybe AppBase
hydrate titles appId = HM.lookup appId titles <&> \(t, v) -> AppBase appId t (iconUrl appId v)

remapAppMgrInfo :: (Elem 'AppMgr2.IncludeDependencies ls ~ 'True, Elem 'AppMgr2.IncludeStatus ls ~ 'True)
                => JobCache
                -> HM.HashMap AppId (AppMgr2.InfoRes ( 'Right ls)) -- ^ AppMgr response
                -> HM.HashMap AppId (AppStatus, Version, AppMgr2.InfoRes ( 'Right ls))
remapAppMgrInfo jobCache serverApps = flip
    HML.mapWithKey
    serverApps
    \appId infoRes@AppMgr2.InfoRes {..} ->
        let refinedDepInfo = flip
                HML.mapWithKey
                infoResDependencies
                \depId depInfo ->
                    case
                            ( HM.lookup depId tmpStatuses
                            , AppMgr2.infoResStatus <$> HM.lookup depId serverApps
                            , AppMgr2.dependencyInfoError depInfo
                            )
                        of
                            -- mute all of the not-running violations that are currently backing up and container is paused
                            (Just CreatingBackup, Just Paused, Just AppMgr2.NotRunning) ->
                                depInfo { AppMgr2.dependencyInfoError = Nothing }
                            (_, _, _) -> depInfo
            realViolations =
                any (isJust . AppMgr2.dependencyInfoError <&&> AppMgr2.dependencyInfoRequired) refinedDepInfo
            (status, version) =
                maybe (AppStatusAppMgr infoResStatus, infoResVersion) (first AppStatusTmp)
                    $   ((, infoResVersion) <$> HM.lookup appId tmpStatuses)
                    <|> (guard (not infoResIsConfigured || infoResIsRecoverable) $> (NeedsConfig, infoResVersion))
                    <|> (guard realViolations $> (BrokenDependencies, infoResVersion))
        in  ( status
            , version
            , infoRes
                { AppMgr2.infoResDependencies = case status of
                                                    AppStatusTmp NeedsConfig -> HM.empty
                                                    _                        -> refinedDepInfo
                }
            )
    where tmpStatuses = statuses jobCache

storeAppToAppBase :: StoreApp -> AppBase
storeAppToAppBase StoreApp {..} =
    AppBase storeAppId storeAppTitle (storeIconUrl storeAppId (storeAppVersionInfoVersion $ extract storeAppVersions))

storeAppToAvailablePreview :: StoreApp -> Maybe (Version, AppStatus) -> AppAvailablePreview
storeAppToAvailablePreview s@StoreApp {..} installed = AppAvailablePreview
    (storeAppToAppBase s)
    (storeAppVersionInfoVersion $ extract storeAppVersions)
    storeAppDescriptionShort
    installed
    storeAppTimestamp

type AsInstalled :: Bool -> Type
newtype AsInstalled a = AsInstalled { unAsInstalled :: SBool a }
dependencyInfoToDependencyRequirement :: AsInstalled a
                                      -> (AppBase, Maybe AppStatus, AppMgr2.DependencyInfo)
                                      -> (AppDependencyRequirement (If a Strip Keep))
dependencyInfoToDependencyRequirement asInstalled (base, status, AppMgr2.DependencyInfo {..}) = do
    let appDependencyRequirementBase        = base
    let appDependencyRequirementDescription = dependencyInfoDescription
    let appDependencyRequirementVersionSpec = dependencyInfoVersionSpec
    let appDependencyRequirementViolation = case (status, dependencyInfoError) of
            (Just s@(AppStatusTmp Installing), _) -> Just $ IncompatibleStatus s
            (Nothing, _                        ) -> Just Missing
            (_      , Just AppMgr2.NotInstalled) -> Just Missing
            (_, Just (AppMgr2.InvalidVersion _ _)) -> Just IncompatibleVersion
            (_, Just (AppMgr2.UnsatisfiedConfig reasons)) -> Just . IncompatibleConfig $ reasons
            (Just s , Just AppMgr2.NotRunning  ) -> Just $ IncompatibleStatus s
            (_      , Nothing                  ) -> Nothing
    case asInstalled of
        AsInstalled STrue ->
            let appDependencyRequirementReasonOptional = ()
                appDependencyRequirementDefault        = ()
            in  AppDependencyRequirement { .. }
        AsInstalled SFalse ->
            let appDependencyRequirementReasonOptional = dependencyInfoReasonOptional
                appDependencyRequirementDefault        = dependencyInfoRequired
            in  AppDependencyRequirement { .. }

postEnableLanR :: AppId -> Handler ()
postEnableLanR = intoHandler . postEnableLanLogic

postEnableLanLogic :: (Has (Reader AgentCtx) sig m, Has AppMgr2.AppMgr sig m, MonadBaseControl IO m, MonadIO m)
                   => AppId
                   -> m ()
postEnableLanLogic appId = do
    cache  <- asks appLanThreads
    action <- const () <<$>> LAsync.async (AppMgr2.lanEnable appId) -- unconditionally drops monad state from the action
    liftIO $ atomically $ modifyTVar' cache (HM.insert appId action)

postDisableLanR :: AppId -> Handler ()
postDisableLanR = intoHandler . postDisableLanLogic

postDisableLanLogic :: (Has (Reader AgentCtx) sig m, MonadBaseControl IO m, MonadIO m) => AppId -> m ()
postDisableLanLogic appId = do
    cache  <- asks appLanThreads
    action <- liftIO . atomically $ stateTVar cache $ \s -> (HM.lookup appId s, HM.delete appId s)
    case action of
        Nothing -> pure () -- Nothing to do here
        Just x  -> LAsync.cancel x
