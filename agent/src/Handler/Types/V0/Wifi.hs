{-# LANGUAGE RecordWildCards #-}
module Handler.Types.V0.Wifi where

import           Startlude

import           Data.Aeson
import           Yesod.Core

data AddWifiReq = AddWifiReq
    { addWifiSsid     :: Text
    , addWifiPassword :: Text
    , addWifiCountry  :: Text
    , skipConnect     :: Bool
    } deriving (Eq, Show)
instance FromJSON AddWifiReq where
    parseJSON = withObject "AddWifiReq" $ \o -> do
        addWifiSsid     <- o .: "ssid"
        addWifiPassword <- o .: "password"
        addWifiCountry  <- o .:? "country" .!= "US"
        skipConnect     <- o .:? "skipConnect" .!= False
        pure AddWifiReq { .. }

data WifiList = WifiList
    { wifiListCurrent :: Maybe Text
    , wifiListSsids   :: [Text]
    } deriving (Eq, Show)
instance ToJSON WifiList where
    toJSON WifiList {..} = object ["current" .= wifiListCurrent, "ssids" .= wifiListSsids]
instance ToTypedContent WifiList where
    toTypedContent = toTypedContent . toJSON
instance ToContent WifiList where
    toContent = toContent . toJSON
