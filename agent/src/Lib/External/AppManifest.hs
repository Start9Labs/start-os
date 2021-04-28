{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.External.AppManifest where

import           Startlude               hiding ( ask )

import           Control.Effect.Reader.Labelled
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.Yaml                     as Yaml

import           Control.Monad.Fail             ( MonadFail(fail) )
import           Lib.Error
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )

data ImageType = ImageTypeTar
    deriving (Eq, Show)

instance FromJSON ImageType where
    parseJSON = withText "Image Type" $ \case
        "tar" -> pure ImageTypeTar
        wat   -> fail $ "Unknown Image Type: " <> toS wat

data OnionVersion = OnionV2 | OnionV3
    deriving (Eq, Ord, Show)

instance FromJSON OnionVersion where
    parseJSON = withText "Onion Version" $ \case
        "v2" -> pure OnionV2
        "v3" -> pure OnionV3
        wat  -> fail $ "Unknown Onion Version: " <> toS wat

data AssetMapping = AssetMapping
    { assetMappingSource    :: FilePath
    , assetMappingDest      :: FilePath
    , assetMappingOverwrite :: Bool
    }
    deriving (Eq, Show)

instance FromJSON AssetMapping where
    parseJSON = withObject "Asset Mapping" $ \o -> do
        assetMappingSource    <- o .: "src"
        assetMappingDest      <- o .: "dst"
        assetMappingOverwrite <- o .: "overwrite"
        pure $ AssetMapping { .. }

data Action = Action
    { actionId              :: Text
    , actionName            :: Text
    , actionDescription     :: Text
    , actionWarning         :: Maybe Text
    , actionAllowedStatuses :: [AppContainerStatus]
    }
    deriving Show
instance FromJSON Action where
    parseJSON = withObject "AppAction" $ \o -> do
        actionId              <- o .: "id"
        actionName            <- o .: "name"
        actionDescription     <- o .: "description"
        actionWarning         <- o .:? "warning"
        actionAllowedStatuses <- o .: "allowed-statuses"
        pure Action { .. }
instance ToJSON Action where
    toJSON Action {..} =
        object
            $  [ "id" .= actionId
               , "name" .= actionName
               , "description" .= actionDescription
               , "allowedStatuses" .= actionAllowedStatuses
               ]
            <> maybeToList (("warning" .=) <$> actionWarning)


data AppManifest where
     AppManifest ::{ appManifestId :: AppId
                    , appManifestVersion :: Version
                    , appManifestTitle :: Text
                    , appManifestLicenseName :: Maybe Text
                    , appManifestLicenseLink :: Maybe Text
                    , appManifestDescShort :: Text
                    , appManifestDescLong :: Text
                    , appManifestReleaseNotes :: Text
                    , appManifestPortMapping :: [PortMapEntry]
                    , appManifestImageType :: ImageType
                    , appManifestMount :: FilePath
                    , appManifestAssets :: [AssetMapping]
                    , appManifestOnionVersion :: OnionVersion
                    , appManifestDependencies :: HM.HashMap AppId VersionRange
                    , appManifestUninstallAlert :: Maybe Text
                    , appManifestRestoreAlert   :: Maybe Text
                    , appManifestStartAlert :: Maybe Text
                    , appManifestActions :: [Action]
                    } -> AppManifest
deriving instance Show AppManifest

torUiAvailable :: AppManifest -> Bool
torUiAvailable AppManifest {..} = any (== 80) $ portMapEntryTor <$> appManifestPortMapping

lanUiAvailable :: AppManifest -> Bool
lanUiAvailable AppManifest {..} = any id $ fmap portMapEntryLan appManifestPortMapping <&> \case
    Just Standard     -> True
    Just (Custom 443) -> True
    Just (Custom 80 ) -> True
    _                 -> False

instance FromJSON AppManifest where
    parseJSON = withObject "App Manifest " $ \o -> do
        appManifestId             <- o .: "id"
        appManifestVersion        <- o .: "version"
        appManifestTitle          <- o .: "title"
        appManifestLicenseName    <- o .:? "license-info" >>= traverse (.: "license")
        appManifestLicenseLink    <- o .:? "license-info" >>= traverse (.: "url")
        appManifestDescShort      <- o .: "description" >>= (.: "short")
        appManifestDescLong       <- o .: "description" >>= (.: "long")
        appManifestReleaseNotes   <- o .: "release-notes"
        appManifestPortMapping    <- o .: "ports"
        appManifestImageType      <- o .: "image" >>= (.: "type")
        appManifestMount          <- o .: "mount"
        appManifestAssets         <- o .: "assets" >>= traverse parseJSON
        appManifestOnionVersion   <- o .: "hidden-service-version"
        appManifestDependencies   <- o .:? "dependencies" .!= HM.empty >>= traverse parseDepInfo
        appManifestUninstallAlert <- o .:? "uninstall-alert"
        appManifestRestoreAlert   <- o .:? "restore-alert"
        appManifestStartAlert     <- o .:? "start-alert"
        appManifestActions        <- o .: "actions"
        pure $ AppManifest { .. }
        where parseDepInfo = withObject "Dep Info" $ (.: "version")

getAppManifest :: (MonadIO m, HasFilesystemBase sig m) => AppId -> S9ErrT m (Maybe AppManifest)
getAppManifest appId = do
    base <- ask @"filesystemBase"
    ExceptT $ first (ManifestParseE appId) <$> liftIO
        (Yaml.decodeFileEither . toS $ (appMgrAppPath appId <> "manifest.yaml") `relativeTo` base)

data LanConfiguration = Standard | Custom Word16 deriving (Eq, Show)
instance FromJSON LanConfiguration where
    parseJSON = liftA2 (<|>) standard custom
        where
            standard =
                withText "Standard Lan" \t -> if t == "standard" then pure Standard else fail "Not Standard Lan Conf"
            custom = withObject "Custom Lan" $ \o -> do
                Custom <$> (o .: "custom" >>= (.: "port"))
data PortMapEntry = PortMapEntry
    { portMapEntryInternal :: Word16
    , portMapEntryTor      :: Word16
    , portMapEntryLan      :: Maybe LanConfiguration
    }
    deriving (Eq, Show)
instance FromJSON PortMapEntry where
    parseJSON = withObject "Port Map Entry" $ \o -> do
        portMapEntryInternal <- o .: "internal"
        portMapEntryTor      <- o .: "tor"
        portMapEntryLan      <- o .:? "lan"
        pure PortMapEntry { .. }
