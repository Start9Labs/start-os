{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.V0 where

import           Startlude               hiding ( runReader )

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift           ( runM )
import           Data.Aeson
import           Data.IORef
import qualified Data.Text                     as T
import           Database.Persist              as Persist
import           Yesod.Core.Handler
import           Yesod.Persist.Core
import           Yesod.Core.Json

import           Constants
import           Daemon.ZeroConf
import           Foundation
import           Handler.Types.V0.Specs
import           Handler.Types.V0.Ssh
import           Handler.Types.V0.Base
import           Handler.Types.V0.Wifi
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import           Lib.External.Metrics.Df
import qualified Lib.External.Registry         as Reg
import           Lib.External.Specs.CPU
import           Lib.External.Specs.Memory
import qualified Lib.External.WpaSupplicant    as WpaSupplicant
import           Lib.Notifications
import           Lib.SystemPaths
import           Lib.Ssh
import           Lib.Tor
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model
import           Settings
import           Util.Function


getServerR :: Handler (JsonEncoding ServerRes)
getServerR = handleS9ErrT $ do
    agentCtx   <- getYesod
    let settings = appSettings agentCtx
    now        <- liftIO getCurrentTime
    isUpdating <- getsYesod appIsUpdating >>= liftIO . readIORef

    let status = if isJust isUpdating then Nothing else Just Running

    notifs <- case isUpdating of
        Nothing -> lift . runDB $ do
            notif <- selectList [NotificationArchivedAt ==. Nothing] [Desc NotificationCreatedAt]
            void . archive . fmap entityKey $ notif
            pure notif
        Just _ -> pure []

    alternativeRegistryUrl <- runM $ injectFilesystemBaseFromContext settings $ readSystemPath altRegistryUrlPath
    name                   <- runM $ injectFilesystemBaseFromContext settings $ readSystemPath serverNamePath
    ssh                    <- readFromPath settings sshKeysFilePath >>= parseSshKeys
    wifi                   <- WpaSupplicant.runWlan0 $ liftA2 WifiList WpaSupplicant.getCurrentNetwork WpaSupplicant.listNetworks
    specs                  <- getSpecs settings
    welcomeAck             <- fmap isJust . lift . runDB . Persist.get $ WelcomeAckKey agentVersion
    versionLatest          <- getOSVersionLatest agentCtx

    let sid = T.drop 7 $ specsNetworkId specs

    jsonEncode ServerRes { serverId                     = specsNetworkId specs
                         , serverName                   = fromMaybe ("Embassy:" <> sid) name
                         , serverStatus                 = AppStatusAppMgr <$> status
                         , serverStatusAt               = now
                         , serverVersionInstalled       = agentVersion
                         , serverNotifications          = notifs
                         , serverWifi                   = wifi
                         , serverSsh                    = ssh
                         , serverAlternativeRegistryUrl = alternativeRegistryUrl
                         , serverSpecs                  = specs
                         , serverWelcomeAck             = welcomeAck
                         , serverVersionLatest           = versionLatest
                         }
    where
        parseSshKeys :: Text -> S9ErrT Handler [SshKeyFingerprint]
        parseSshKeys keysContent = do
            let keys = lines . T.strip $ keysContent
            case traverse fingerprint keys of
                Left  e  -> throwE $ InvalidSshKeyE (toS e)
                Right as -> pure $ uncurry3 SshKeyFingerprint <$> as

postWelcomeR :: Version -> Handler ()
postWelcomeR version = runDB $ repsert (WelcomeAckKey version) WelcomeAck

getSpecs :: MonadIO m => AppSettings -> S9ErrT m SpecsRes
getSpecs settings = do
    specsCPU        <- liftIO getCpuInfo
    specsMem        <- liftIO getMem
    specsDisk       <- fmap show . metricDiskSize <$> getDfMetrics
    specsNetworkId  <- runM $ injectFilesystemBaseFromContext settings getStart9AgentHostname
    specsTorAddress <- runM $ injectFilesystemBaseFromContext settings getAgentHiddenServiceUrl

    let specsAgentVersion = agentVersion
    pure $ SpecsRes { .. }

readFromPath :: MonadIO m => AppSettings -> SystemPath -> S9ErrT m Text
readFromPath settings sp = runM (injectFilesystemBaseFromContext settings (readSystemPath sp)) >>= \case
    Nothing  -> throwE $ MissingFileE sp
    Just res -> pure res

--------------------- UPDATES TO SERVER -------------------------

newtype PatchReq = PatchReq { patchValue :: Text } deriving(Eq, Show)
instance FromJSON PatchReq where
    parseJSON = withObject "Patch Request" $ \o -> PatchReq <$> o .: "value"

newtype NullablePatchReq = NullablePatchReq { mpatchValue :: Maybe Text } deriving(Eq, Show)
instance FromJSON NullablePatchReq where
    parseJSON = withObject "Nullable Patch Request" $ \o -> NullablePatchReq <$> o .:? "value"

newtype BoolPatchReq = BoolPatchReq { bpatchValue :: Bool } deriving (Eq, Show)

instance FromJSON BoolPatchReq where
  parseJSON = withObject "Patch Request" $ \o -> BoolPatchReq <$> o .: "value"

patchNameR :: Handler ()
patchNameR = patchFile serverNamePath

patchAutoCheckUpdatesR :: Handler ()
patchAutoCheckUpdatesR = do
  settings <- getsYesod appSettings
  BoolPatchReq val <- requireCheckJsonBody
  runM $
    injectFilesystemBaseFromContext settings $
      if val
        then writeSystemPath autoCheckUpdatesPath ""
        else deleteSystemPath autoCheckUpdatesPath

patchFile :: SystemPath -> Handler ()
patchFile path = do
    settings     <- getsYesod appSettings
    PatchReq val <- requireCheckJsonBody
    runM $ injectFilesystemBaseFromContext settings $ writeSystemPath path val

patchNullableFile :: SystemPath -> Handler ()
patchNullableFile path = do
    settings              <- getsYesod appSettings
    NullablePatchReq mVal <- requireCheckJsonBody
    runM $ injectFilesystemBaseFromContext settings $ case mVal of
        Just val -> writeSystemPath path $ T.strip val
        Nothing  -> deleteSystemPath path

expirationOsVersionLatest :: Num a => a
expirationOsVersionLatest = 60

getOSVersionLatest :: MonadIO m => AgentCtx -> m (Maybe Version)
getOSVersionLatest ctx = do
    now <- liftIO getCurrentTime
    let osVersionCache = appOSVersionLatest ctx
    mCache <- liftIO . readIORef $ osVersionCache

    case mCache of
        Nothing -> repopulateCache ctx
        Just cache -> if diffUTCTime now (lastChecked cache) >= expirationOsVersionLatest
            then repopulateCache ctx
            else pure . Just $ osVersion cache

repopulateCache :: MonadIO m => AgentCtx -> m (Maybe Version)
repopulateCache ctx = do
    let osVersionCache = appOSVersionLatest ctx
    let s = appSettings ctx
    eitherV <- interp s $ Reg.getLatestAgentVersion

    case eitherV of
        Left error -> do
            putStrLn $ "Repopulate OS Version Cache exception: " <> (show error :: Text)
            fmap (fmap osVersion) . liftIO . readIORef $ osVersionCache
        Right v -> do
            res <- OsVersionCache v <$> liftIO getCurrentTime
            liftIO $ writeIORef osVersionCache (Just res)
            pure . Just $ osVersion res
    where interp s = liftIO . runError @S9Error . injectFilesystemBaseFromContext s . runRegistryUrlIOC

