{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Error where

import           Startlude

import           Control.Carrier.Error.Church
import           Data.Aeson              hiding ( Error )
import           Data.String.Interpolate.IsString
import qualified Data.Yaml                     as Yaml
import qualified GHC.Show                       ( Show(..) )
import           Network.HTTP.Types
import           System.Process
import           Yesod.Core              hiding ( ErrorResponse )

import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver


type S9ErrT m = ExceptT S9Error m

data S9Error =
      ProductKeyE
    | RegistrationE
    | NoCompliantAgentE VersionRange
    | PersistentE Text
    | WifiConnectionE
    | AppMgrParseE Text Text String
    | AppMgrInvalidConfigE Text
    | AppMgrE Text Int
    | AvahiE Text
    | MetricE Text
    | AppMgrVersionE Version VersionRange
    | RegistryUnreachableE
    | RegistryParseE Text Text
    | AppNotInstalledE AppId
    | AppStateActionIncompatibleE AppId AppStatus AppAction
    | UpdateSelfE UpdateSelfStep Text
    | InvalidSshKeyE Text
    | InvalidSsidE
    | InvalidPskE
    | InvalidRequestE Value Text
    | NotFoundE Text Text
    | UpdateInProgressE
    | TemporarilyForbiddenE AppId Text Text
    | TorServiceTimeoutE
    | NginxSslE Text
    | WifiOrphaningE
    | NoPasswordExistsE
    | HostsParamsE Text
    | MissingFileE SystemPath
    | ClientCryptographyE Text
    | TTLExpirationE Text
    | ManifestParseE AppId Yaml.ParseException
    | EnvironmentValE AppId
    | InternalE Text
    | BackupE AppId Text
    | BackupPassInvalidE
    | OpenSslE Text Int String String
data UpdateSelfStep =
      GetLatestCompliantVersion
    | GetYoungAgentBinary
    | ShutdownWeb
    | StartupYoungAgent
    | PingYoungAgent ProcessHandle
instance Show S9Error where
    show = show . toError

instance Exception S9Error

newtype InternalS9Error = InternalS9Error Text deriving (Eq, Show)
instance Exception InternalS9Error

-- | Redact any sensitive data in this function
toError :: S9Error -> ErrorResponse
toError = \case
    ProductKeyE            -> ErrorResponse PRODUCT_KEY_ERROR "The product key is invalid"
    RegistrationE          -> ErrorResponse REGISTRATION_ERROR "The product already has an owner"
    NoCompliantAgentE spec -> ErrorResponse AGENT_UPDATE_ERROR [i|No valid agent version for spec #{spec}|]
    PersistentE       t    -> ErrorResponse DATABASE_ERROR t
    WifiConnectionE        -> ErrorResponse WIFI_ERROR "Could not connect to wifi"
    AppMgrInvalidConfigE e -> ErrorResponse APPMGR_CONFIG_ERROR e
    AppMgrParseE cmd result e ->
        ErrorResponse APPMGR_PARSE_ERROR [i|"appmgr #{cmd}" yielded an unparseable result:#{result}\nError: #{e}|]
    AppMgrE cmd code -> ErrorResponse APPMGR_ERROR [i|"appmgr #{cmd}" exited with #{code}|]
    AppMgrVersionE av avs ->
        ErrorResponse APPMGR_ERROR [i|"appmgr version #{av}" fails to satisfy requisite spec #{avs}|]
    AvahiE  e               -> ErrorResponse AVAHI_ERROR [i|#{e}|]
    MetricE m               -> ErrorResponse METRICS_ERROR [i|failed to provide metrics: #{m}|]
    RegistryUnreachableE    -> ErrorResponse REGISTRY_ERROR [i|registry is unreachable|]
    RegistryParseE path msg -> ErrorResponse REGISTRY_ERROR [i|registry "#{path}" failed to parse: #{msg}|]
    AppNotInstalledE appId  -> ErrorResponse APP_NOT_INSTALLED [i|#{appId} is not installed|]
    AppStateActionIncompatibleE appId status action -> ErrorResponse APP_ACTION_FORBIDDEN $ case (status, action) of
        (AppStatusAppMgr Dead, _) -> [i|#{appId} cannot be #{action}ed because it is dead...contact support?|]
        (AppStatusAppMgr Removing, _) -> [i|#{appId} cannot be #{action}ed because it is being removed|]
        (AppStatusAppMgr Running, Start) -> [i|#{appId} is already running|]
        (AppStatusAppMgr Stopped, Stop) -> [i|#{appId} is already stopped|]
        (AppStatusAppMgr Restarting, Start) -> [i|#{appId} is already running|]
        (AppStatusAppMgr Running, Stop) -> [i|Running apps should be stoppable, this is a bug, contact support|]
        (AppStatusAppMgr Stopped, Start) -> [i|Stopped apps should be startable, this is a bug, contact support|]
        (AppStatusAppMgr Restarting, Stop) -> [i|Restarting apps should be stoppable, this is a bug, contact support|]
        (AppStatusAppMgr Paused, _) -> [i|Paused is not an externally visible state, this is a bug, contact support|]
        (AppStatusTmp NeedsConfig, Start) -> [i|#{appId} cannot be started because it is not configured|]
        (AppStatusTmp NeedsConfig, Stop) -> [i|#{appId} is already stopped|]
        (AppStatusTmp BrokenDependencies, Start) -> [i|Cannot start service: Dependency Issue|]
        (AppStatusTmp _, _) -> [i|Cannot issue control actions to apps in temporary states|]
    UpdateSelfE step e -> ErrorResponse SELF_UPDATE_ERROR $ case step of
        GetLatestCompliantVersion -> [i|could not find a compliant version for the specification|]
        GetYoungAgentBinary       -> [i|could not get young agent binary: #{e}|]
        ShutdownWeb               -> [i|could not shutdown web: #{e}|]
        StartupYoungAgent         -> [i|could not startup young agent: #{e}|]
        PingYoungAgent _          -> [i|could not ping young agent: #{e}|]
    InvalidSshKeyE key -> ErrorResponse INVALID_SSH_KEY [i|The ssh key "#{key}" is invalid|]
    InvalidSsidE -> ErrorResponse INVALID_SSID [i|The ssid is invalid. Only ASCII characters allowed.|]
    InvalidPskE -> ErrorResponse INVALID_SSID [i|The wifi password is invalid. Only ASCII characters allowed.|]
    InvalidRequestE val reason -> ErrorResponse INVALID_REQUEST [i|The body #{encode val} is invalid: #{reason}|]
    NotFoundE resource val -> ErrorResponse RESOURCE_NOT_FOUND [i|The #{resource} #{val} was not found|]
    UpdateInProgressE ->
        ErrorResponse UPDATE_IN_PROGRESS [i|Your request could not be completed because your server is updating|]
    TemporarilyForbiddenE appId action st ->
        ErrorResponse APP_ACTION_FORBIDDEN [i|The #{action} for #{appId} is temporarily forbidden because it is #{st}|]
    TorServiceTimeoutE ->
        ErrorResponse INTERNAL_ERROR [i|The MeshOS Tor Service could not be started...contact support|]
    NginxSslE e    -> ErrorResponse INTERNAL_ERROR [i|MeshOS could not be started with SSL #{e}|]
    WifiOrphaningE -> ErrorResponse
        WIFI_ERROR
        [i|You cannot delete the wifi network you are currently connected to unless on ethernet|]
    ManifestParseE appId e ->
        ErrorResponse INTERNAL_ERROR [i|There was an error inspecting the manifest for #{appId}: #{e}|]
    NoPasswordExistsE         -> ErrorResponse REGISTRATION_ERROR [i|Unauthorized. No password has been registered|]
    MissingFileE        sp    -> ErrorResponse RESOURCE_NOT_FOUND [i|File not found as #{leaf sp}|]
    ClientCryptographyE desc  -> ErrorResponse REGISTRATION_ERROR [i|Cryptography failure: #{desc}|]
    TTLExpirationE      desc  -> ErrorResponse REGISTRATION_ERROR [i|TTL Expiration failure: #{desc}|]
    EnvironmentValE     appId -> ErrorResponse SYNCHRONIZATION_ERROR [i|Could not read environment values for #{appId}|]
    HostsParamsE        key   -> ErrorResponse REGISTRATION_ERROR [i|Missing or invalid parameter #{key}|]
    InternalE           msg   -> ErrorResponse INTERNAL_ERROR msg
    BackupE appId reason      -> ErrorResponse BACKUP_ERROR [i|Backup failed for #{appId}: #{reason}|]
    BackupPassInvalidE        -> ErrorResponse BACKUP_ERROR [i|Password provided for backups is invalid|]
    OpenSslE cert ec stdout' stderr' ->
        ErrorResponse OPENSSL_ERROR [i|OPENSSL ERROR: #{cert} - #{show ec <> "\n" <> stdout' <> "\n" <> stderr'}|]

data ErrorCode =
      PRODUCT_KEY_ERROR
    | REGISTRATION_ERROR
    | AGENT_UPDATE_ERROR
    | DATABASE_ERROR
    | WIFI_ERROR
    | APPMGR_CONFIG_ERROR
    | APPMGR_PARSE_ERROR
    | APPMGR_ERROR
    | AVAHI_ERROR
    | REGISTRY_ERROR
    | APP_NOT_INSTALLED
    | APP_NOT_CONFIGURED
    | APP_ACTION_FORBIDDEN
    | SELF_UPDATE_ERROR
    | INVALID_SSH_KEY
    | INVALID_SSID
    | INVALID_PSK
    | INVALID_REQUEST
    | INVALID_HEADER
    | MISSING_HEADER
    | METRICS_ERROR
    | RESOURCE_NOT_FOUND
    | UPDATE_IN_PROGRESS
    | INTERNAL_ERROR
    | SYNCHRONIZATION_ERROR
    | BACKUP_ERROR
    | OPENSSL_ERROR
    deriving (Eq, Show)
instance ToJSON ErrorCode where
    toJSON = String . show

data ErrorResponse = ErrorResponse
    { errorCode    :: ErrorCode
    , errorMessage :: Text
    }
    deriving (Eq, Show)
instance ToJSON ErrorResponse where
    toJSON ErrorResponse {..} = object ["code" .= errorCode, "message" .= errorMessage]
instance ToContent ErrorResponse where
    toContent = toContent . toJSON
instance ToTypedContent ErrorResponse where
    toTypedContent = toTypedContent . toJSON

instance ToTypedContent S9Error where
    toTypedContent = toTypedContent . toJSON . toError
instance ToContent S9Error where
    toContent = toContent . toJSON . toError

toStatus :: S9Error -> Status
toStatus = \case
    ProductKeyE            -> status401
    RegistrationE          -> status403
    NoCompliantAgentE _    -> status404
    PersistentE       _    -> status500
    WifiConnectionE        -> status500
    AppMgrParseE _ _ _     -> status500
    AppMgrInvalidConfigE _ -> status400
    AppMgrE        _ _     -> status500
    AppMgrVersionE _ _     -> status500
    AvahiE  _              -> status500
    MetricE _              -> status500
    RegistryUnreachableE   -> status500
    RegistryParseE _ _     -> status500
    AppNotInstalledE _     -> status404
    AppStateActionIncompatibleE _ status action -> case (status, action) of
        (AppStatusAppMgr Dead       , _    ) -> status500
        (AppStatusAppMgr Removing   , _    ) -> status403
        (AppStatusAppMgr Running    , Start) -> status200
        (AppStatusAppMgr Running    , Stop ) -> status200
        (AppStatusAppMgr Restarting , Start) -> status200
        (AppStatusAppMgr Restarting , Stop ) -> status200
        (AppStatusAppMgr Stopped    , Start) -> status200
        (AppStatusAppMgr Stopped    , Stop ) -> status200
        (AppStatusAppMgr Paused     , _    ) -> status403
        (AppStatusTmp    NeedsConfig, Start) -> status403
        (AppStatusTmp    NeedsConfig, Stop ) -> status200
        (AppStatusTmp    _          , _    ) -> status403
    UpdateSelfE _ _             -> status500
    InvalidSshKeyE _            -> status400
    InvalidSsidE                -> status400
    InvalidPskE                 -> status400
    InvalidRequestE _ _         -> status400
    NotFoundE       _ _         -> status404
    UpdateInProgressE           -> status403
    TemporarilyForbiddenE _ _ _ -> status403
    TorServiceTimeoutE          -> status500
    NginxSslE _                 -> status500
    WifiOrphaningE              -> status403
    ManifestParseE _ _          -> status500
    NoPasswordExistsE           -> status401
    MissingFileE        _       -> status500
    ClientCryptographyE _       -> status401
    TTLExpirationE      _       -> status403
    EnvironmentValE     _       -> status500
    HostsParamsE        _       -> status400
    BackupE _ _                 -> status500
    BackupPassInvalidE          -> status403
    InternalE _                 -> status500
    OpenSslE _ _ _ _            -> status500

handleS9ErrC :: (MonadHandler m, MonadLogger m) => ErrorC S9Error m a -> m a
handleS9ErrC action =
    let handleIt e = do
            $logError $ show e
            toStatus >>= sendResponseStatus $ e
    in  runErrorC action handleIt pure

handleS9ErrT :: (MonadHandler m, MonadLogger m) => S9ErrT m a -> m a
handleS9ErrT action = do
    runExceptT action >>= \case
        Left e -> do
            $logError $ show e
            toStatus >>= sendResponseStatus $ e
        Right a -> pure a

runS9ErrT :: MonadIO m => S9ErrT m a -> m (Either S9Error a)
runS9ErrT = runExceptT

logS9ErrT :: (MonadIO m, MonadLogger m) => S9ErrT m a -> m (Maybe a)
logS9ErrT x = runS9ErrT x >>= \case
    Left e -> do
        $logError $ show e
        pure Nothing
    Right a -> pure $ Just a

handleS9ErrNuclear :: MonadIO m => S9ErrT m a -> m a
handleS9ErrNuclear action = runExceptT action >>= \case
    Left  e -> throwIO e
    Right a -> pure a

orThrowM :: Has (Error e) sig m => m (Maybe a) -> e -> m a
orThrowM action e = action >>= maybe (throwError e) pure
{-# INLINE orThrowM #-}

orThrowPure :: Has (Error e) sig m => Maybe a -> e -> m a
orThrowPure thing e = maybe (throwError e) pure thing
{-# INLINE orThrowPure #-}

