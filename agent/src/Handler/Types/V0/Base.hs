{-# LANGUAGE RecordWildCards #-}
module Handler.Types.V0.Base where

import           Startlude

import           Data.Aeson
import           Database.Persist
import           Yesod.Core

import           Handler.Types.V0.Ssh
import           Handler.Types.V0.Specs
import           Handler.Types.V0.Wifi
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model

data VersionLatestRes = VersionLatestRes
    { versionLatestVersion :: Version
    }
    deriving (Eq, Show)
instance ToJSON VersionLatestRes where
    toJSON VersionLatestRes {..} = object $ ["versionLatest" .= versionLatestVersion]
instance ToTypedContent VersionLatestRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent VersionLatestRes where
    toContent = toContent . toJSON

data ServerRes = ServerRes
    { serverId                     :: Text
    , serverName                   :: Text
    , serverStatus                 :: Maybe AppStatus
    , serverStatusAt               :: UTCTime
    , serverVersionInstalled       :: Version
    , serverNotifications          :: [Entity Notification]
    , serverWifi                   :: WifiList
    , serverSsh                    :: [SshKeyFingerprint]
    , serverAlternativeRegistryUrl :: Maybe Text
    , serverSpecs                  :: SpecsRes
    , serverWelcomeAck             :: Bool
    }
    deriving (Eq, Show)

type JsonEncoding a = Encoding
jsonEncode :: (Monad m, ToJSON a) => a -> m (JsonEncoding a)
jsonEncode = returnJsonEncoding

instance ToJSON ServerRes where
    toJSON ServerRes {..} = object
        [ "serverId" .= serverId
        , "name" .= serverName
        , "status" .= case serverStatus of
            Nothing   -> String "UPDATING"
            Just stat -> toJSON stat
        , "versionInstalled" .= serverVersionInstalled
        , "versionLatest" .= Null
        , "notifications" .= serverNotifications
        , "wifi" .= serverWifi
        , "ssh" .= serverSsh
        , "alternativeRegistryUrl" .= serverAlternativeRegistryUrl
        , "specs" .= serverSpecs
        , "welcomeAck" .= serverWelcomeAck
        ]
instance ToTypedContent ServerRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent ServerRes where
    toContent = toContent . toJSON

newtype AppVersionRes = AppVersionRes
    { unAppVersionRes :: Version } deriving (Eq, Show)
instance ToJSON AppVersionRes where
    toJSON AppVersionRes { unAppVersionRes } = object ["version" .= unAppVersionRes]
instance FromJSON AppVersionRes where
    parseJSON = withObject "app version response" $ \o -> do
        av <- o .: "version"
        pure $ AppVersionRes av
instance ToContent AppVersionRes where
    toContent = toContent . toJSON
instance ToTypedContent AppVersionRes where
    toTypedContent = toTypedContent . toJSON
