{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Apps where

import           Startlude

import           Data.Aeson
import           Data.Aeson.Flatten
import           Data.Singletons

import           Lib.TyFam.ConditionalData
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )
import           Lib.Types.NetAddress
data AppBase = AppBase
    { appBaseId      :: AppId
    , appBaseTitle   :: Text
    , appBaseIconUrl :: Text
    }
    deriving (Eq, Show)
instance ToJSON AppBase where
    toJSON AppBase {..} = object ["id" .= appBaseId, "title" .= appBaseTitle, "iconURL" .= appBaseIconUrl]

data AppAvailablePreview = AppAvailablePreview
    { appAvailablePreviewBase             :: AppBase
    , appAvailablePreviewVersionLatest    :: Version
    , appAvailablePreviewDescriptionShort :: Text
    , appAvailablePreviewInstallInfo      :: Maybe (Version, AppStatus)
    }
    deriving (Eq, Show)
instance ToJSON AppAvailablePreview where
    toJSON AppAvailablePreview {..} = mergeTo (toJSON appAvailablePreviewBase) $ object
        [ "versionLatest" .= appAvailablePreviewVersionLatest
        , "descriptionShort" .= appAvailablePreviewDescriptionShort
        , "versionInstalled" .= (fst <$> appAvailablePreviewInstallInfo)
        , "status" .= (snd <$> appAvailablePreviewInstallInfo)
        ]

data AppInstalledPreview = AppInstalledPreview
    { appInstalledPreviewBase             :: AppBase
    , appInstalledPreviewStatus           :: AppStatus
    , appInstalledPreviewVersionInstalled :: Version
    , appInstalledPreviewTorAddress       :: Maybe TorAddress
    , appInstalledPreviewUi               :: Bool
    }
    deriving (Eq, Show)
instance ToJSON AppInstalledPreview where
    toJSON AppInstalledPreview {..} = mergeTo (toJSON appInstalledPreviewBase) $ object
        [ "status" .= appInstalledPreviewStatus
        , "versionInstalled" .= appInstalledPreviewVersionInstalled
        , "torAddress" .= (unTorAddress <$> appInstalledPreviewTorAddress)
        , "ui" .= appInstalledPreviewUi
        ]

data InstallNewAppReq = InstallNewAppReq
    { installNewAppVersion :: Version
    , installNewAppDryRun  :: Bool
    }
    deriving (Eq, Show)
instance FromJSON InstallNewAppReq where
    parseJSON = withObject "Install New App Request" $ \o -> do
        installNewAppVersion <- o .: "version"
        installNewAppDryRun  <- o .:? "dryRun" .!= False
        pure InstallNewAppReq { .. }

data AppAvailableFull = AppAvailableFull
    { appAvailableFullBase                   :: AppBase
    , appAvailableFullInstallInfo            :: Maybe (Version, AppStatus)
    , appAvailableFullVersionLatest          :: Version
    , appAvailableFullDescriptionShort       :: Text
    , appAvailableFullDescriptionLong        :: Text
    , appAvailableFullReleaseNotes           :: Text
    , appAvailableFullDependencyRequirements :: [Full AppDependencyRequirement]
    , appAvailableFullVersions               :: NonEmpty Version
    }
    -- deriving Eq
instance ToJSON AppAvailableFull where
    toJSON AppAvailableFull {..} = mergeTo
        (toJSON appAvailableFullBase)
        (object
            [ "versionInstalled" .= fmap fst appAvailableFullInstallInfo
            , "status" .= fmap snd appAvailableFullInstallInfo
            , "versionLatest" .= appAvailableFullVersionLatest
            , "descriptionShort" .= appAvailableFullDescriptionShort
            , "descriptionLong" .= appAvailableFullDescriptionLong
            , "versions" .= appAvailableFullVersions
            , "releaseNotes" .= appAvailableFullReleaseNotes
            , "serviceRequirements" .= appAvailableFullDependencyRequirements
            ]
        )

type AppDependencyRequirement :: (Type ~> Type) -> Type
data AppDependencyRequirement f = AppDependencyRequirement
    { appDependencyRequirementBase           :: AppBase
    , appDependencyRequirementReasonOptional :: Apply f (Maybe Text)
    , appDependencyRequirementDefault        :: Apply f Bool
    , appDependencyRequirementDescription    :: Maybe Text
    , appDependencyRequirementViolation      :: Maybe ApiDependencyViolation
    , appDependencyRequirementVersionSpec    :: VersionRange
    }
instance ToJSON (AppDependencyRequirement Strip) where
    toJSON AppDependencyRequirement {..} = mergeTo (toJSON appDependencyRequirementBase) $ object
        [ "versionSpec" .= appDependencyRequirementVersionSpec
        , "description" .= appDependencyRequirementDescription
        , "violation" .= appDependencyRequirementViolation
        ]
instance ToJSON (AppDependencyRequirement Keep) where
    toJSON r =
        let stripped = r { appDependencyRequirementReasonOptional = (), appDependencyRequirementDefault = () }
        in
            mergeTo
                (toJSON @(AppDependencyRequirement Strip) stripped)
                (object
                    [ "optional" .= appDependencyRequirementReasonOptional r
                    , "default" .= appDependencyRequirementDefault r
                    ]
                )

-- filter non required dependencies in installed show
-- mute violations downstream of version for installing apps
data AppInstalledFull = AppInstalledFull
    { appInstalledFullBase                   :: AppBase
    , appInstalledFullStatus                 :: AppStatus
    , appInstalledFullVersionInstalled       :: Version
    , appInstalledFullTorAddress             :: Maybe TorAddress
    , appInstalledFullInstructions           :: Maybe Text
    , appInstalledFullLastBackup             :: Maybe UTCTime
    , appInstalledFullConfiguredRequirements :: [Stripped AppDependencyRequirement]
    }
instance ToJSON AppInstalledFull where
    toJSON AppInstalledFull {..} = object
        [ "instructions" .= appInstalledFullInstructions
        , "lastBackup" .= appInstalledFullLastBackup
        , "configuredRequirements" .= appInstalledFullConfiguredRequirements
        , "torAddress" .= (unTorAddress <$> appInstalledFullTorAddress)
        , "id" .= appBaseId appInstalledFullBase
        , "title" .= appBaseTitle appInstalledFullBase
        , "iconURL" .= appBaseIconUrl appInstalledFullBase
        , "versionInstalled" .= appInstalledFullVersionInstalled
        , "status" .= appInstalledFullStatus
        ]

data AppVersionInfo = AppVersionInfo
    { appVersionInfoVersion                :: Version
    , appVersionInfoReleaseNotes           :: Text
    , appVersionInfoDependencyRequirements :: [Full AppDependencyRequirement]
    }
instance ToJSON AppVersionInfo where
    toJSON AppVersionInfo {..} = object
        [ "version" .= appVersionInfoVersion
        , "releaseNotes" .= appVersionInfoReleaseNotes
        , "serviceRequirements" .= appVersionInfoDependencyRequirements
        ]

data ApiDependencyViolation
    = Missing
    | IncompatibleVersion
    | IncompatibleConfig [Text] -- rule violations
    | IncompatibleStatus AppStatus

instance ToJSON ApiDependencyViolation where
    toJSON Missing             = object ["name" .= ("missing" :: Text)]
    toJSON IncompatibleVersion = object ["name" .= ("incompatible-version" :: Text)]
    toJSON (IncompatibleConfig ruleViolations) =
        object ["name" .= ("incompatible-config" :: Text), "ruleViolations" .= ruleViolations]
    toJSON (IncompatibleStatus status) = object ["name" .= ("incompatible-status" :: Text), "status" .= status]

data WithBreakages a = WithBreakages [AppBase] a
instance {-# Overlappable #-} ToJSON a => ToJSON (WithBreakages a) where
    toJSON (WithBreakages breakages thing) = mergeTo (toJSON thing) (object ["breakages" .= breakages])
instance ToJSON (WithBreakages ()) where
    toJSON (WithBreakages breakages _) = object ["breakages" .= breakages]

newtype AutoconfigureChangesRes = AutoconfigureChangesRes
    { autoconfigureChangesConfig :: Maybe Value
    }
instance ToJSON AutoconfigureChangesRes where
    toJSON AutoconfigureChangesRes {..} = object ["config" .= autoconfigureChangesConfig]
