{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.External.AppManifest where

import           Startlude               hiding ( ask )

import           Control.Effect.Reader.Labelled
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.Yaml                     as Yaml

import           Lib.Error
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )
import           Control.Monad.Fail             ( MonadFail(fail) )

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

data AppManifest where
     AppManifest ::{ appManifestId :: AppId
                    , appManifestVersion :: Version
                    , appManifestTitle :: Text
                    , appManifestDescShort :: Text
                    , appManifestDescLong :: Text
                    , appManifestReleaseNotes :: Text
                    , appManifestPortMapping :: HM.HashMap Word16 Word16
                    , appManifestImageType :: ImageType
                    , appManifestMount :: FilePath
                    , appManifestAssets :: [AssetMapping]
                    , appManifestOnionVersion :: OnionVersion
                    , appManifestDependencies :: HM.HashMap AppId VersionRange
                    , appManifestUninstallWarning :: Maybe Text
                    } -> AppManifest

uiAvailable :: AppManifest -> Bool
uiAvailable AppManifest {..} = isJust $ HM.lookup 80 appManifestPortMapping

instance FromJSON AppManifest where
    parseJSON = withObject "App Manifest " $ \o -> do
        appManifestId               <- o .: "id"
        appManifestVersion          <- o .: "version"
        appManifestTitle            <- o .: "title"
        appManifestDescShort        <- o .: "description" >>= (.: "short")
        appManifestDescLong         <- o .: "description" >>= (.: "long")
        appManifestReleaseNotes     <- o .: "release-notes"
        appManifestPortMapping      <- o .: "ports" >>= fmap HM.fromList . traverse parsePortMapping
        appManifestImageType        <- o .: "image" >>= (.: "type")
        appManifestMount            <- o .: "mount"
        appManifestAssets           <- o .: "assets" >>= traverse parseJSON
        appManifestOnionVersion     <- o .: "hidden-service-version"
        appManifestDependencies     <- o .:? "dependencies" .!= HM.empty >>= traverse parseDepInfo
        appManifestUninstallWarning <- o .:? "uninstall-warning"
        pure $ AppManifest { .. }
        where
            parsePortMapping = withObject "Port Mapping" $ \o -> liftA2 (,) (o .: "tor") (o .: "internal")
            parseDepInfo     = withObject "Dep Info" $ (.: "version")

getAppManifest :: (MonadIO m, HasFilesystemBase sig m) => AppId -> S9ErrT m (Maybe AppManifest)
getAppManifest appId = do
    base <- ask @"filesystemBase"
    ExceptT $ first (ManifestParseE appId) <$> liftIO
        (Yaml.decodeFileEither . toS $ (appMgrAppPath appId <> "manifest.yaml") `relativeTo` base)
