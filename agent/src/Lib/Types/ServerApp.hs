{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.Types.ServerApp where

import           Startlude               hiding ( break )

import           Data.Aeson

import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )

data StoreApp = StoreApp
    { storeAppId               :: AppId
    , storeAppTitle            :: Text
    , storeAppDescriptionShort :: Text
    , storeAppDescriptionLong  :: Text
    , storeAppIconUrl          :: Text
    , storeAppVersions         :: NonEmpty StoreAppVersionInfo
    , storeAppTimestamp        :: UTCTime
    }
    deriving (Eq, Show)

data StoreAppVersionInfo = StoreAppVersionInfo
    { storeAppVersionInfoVersion      :: Version
    , storeAppVersionInfoReleaseNotes :: Text
    , storeAppVersionInfoInstallAlert :: Maybe Text
    }
    deriving (Eq, Show)
instance Ord StoreAppVersionInfo where
    compare = compare `on` storeAppVersionInfoVersion
instance FromJSON StoreAppVersionInfo where
    parseJSON = withObject "Store App Version Info" $ \o -> do
        storeAppVersionInfoVersion      <- o .: "version"
        storeAppVersionInfoReleaseNotes <- o .: "release-notes"
        storeAppVersionInfoInstallAlert <- o .: "install-alert"
        pure StoreAppVersionInfo { .. }
instance ToJSON StoreAppVersionInfo where
    toJSON StoreAppVersionInfo {..} =
        object ["version" .= storeAppVersionInfoVersion, "releaseNotes" .= storeAppVersionInfoReleaseNotes]
