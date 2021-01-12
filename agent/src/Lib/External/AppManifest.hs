{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.External.AppManifest where

import           Startlude               hiding ( ask )

import           Control.Effect.Reader.Labelled
import           Data.Aeson
import           Data.Singletons.TypeLits
import qualified Data.HashMap.Strict           as HM
import qualified Data.Yaml                     as Yaml
import           Exinst

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

data AppManifest (n :: Nat) where
    AppManifestV0 ::{ appManifestV0Id :: AppId
                    , appManifestV0Version :: Version
                    , appManifestV0Title :: Text
                    , appManifestV0DescShort :: Text
                    , appManifestV0DescLong :: Text
                    , appManifestV0ReleaseNotes :: Text
                    , appManifestV0PortMapping :: HM.HashMap Word16 Word16
                    , appManifestV0ImageType :: ImageType
                    , appManifestV0Mount :: FilePath
                    , appManifestV0Assets :: [AssetMapping]
                    , appManifestV0OnionVersion :: OnionVersion
                    , appManifestV0Dependencies :: HM.HashMap AppId VersionRange
                    } -> AppManifest 0

hasUi :: forall n. AppManifest n -> Bool
hasUi AppManifestV0 {..} = isJust $ HM.lookup 80 appManifestV0PortMapping

instance FromJSON (Some1 AppManifest) where
    parseJSON = withObject "App Manifest" $ \o -> Some1 (SNat @0) <$> parseJSON (Object o)


instance FromJSON (AppManifest 0) where
    parseJSON = withObject "App Manifest V0" $ \o -> do
        appManifestV0Id           <- o .: "id"
        appManifestV0Version      <- o .: "version"
        appManifestV0Title        <- o .: "title"
        appManifestV0DescShort    <- o .: "description" >>= (.: "short")
        appManifestV0DescLong     <- o .: "description" >>= (.: "long")
        appManifestV0ReleaseNotes <- o .: "release-notes"
        appManifestV0PortMapping  <- o .: "ports" >>= fmap HM.fromList . traverse parsePortMapping
        appManifestV0ImageType    <- o .: "image" >>= (.: "type")
        appManifestV0Mount        <- o .: "mount"
        appManifestV0Assets       <- o .: "assets" >>= traverse parseJSON
        appManifestV0OnionVersion <- o .: "hidden-service-version"
        appManifestV0Dependencies <- o .:? "dependencies" .!= HM.empty >>= traverse parseDepInfo
        pure $ AppManifestV0 { .. }
        where
            parsePortMapping = withObject "Port Mapping" $ \o -> liftA2 (,) (o .: "tor") (o .: "internal")
            parseDepInfo     = withObject "Dep Info" $ (.: "version")

getAppManifest :: (MonadIO m, HasFilesystemBase sig m) => AppId -> S9ErrT m (Maybe (Some1 AppManifest))
getAppManifest appId = do
    base <- ask @"filesystemBase"
    ExceptT $ first (ManifestParseE appId) <$> liftIO
        (Yaml.decodeFileEither . toS $ (appMgrAppPath appId <> "manifest.yaml") `relativeTo` base)

uiAvailable :: AppManifest n -> Bool
uiAvailable = \case
    AppManifestV0 { appManifestV0PortMapping } -> elem 80 (HM.keys appManifestV0PortMapping)
