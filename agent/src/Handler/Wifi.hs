{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Wifi where

import           Startlude

import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import           Network.HTTP.Types
import           Yesod.Core

import           Constants
import           Foundation
import           Handler.Types.V0.Wifi
import           Lib.Error
import qualified Lib.External.WpaSupplicant    as WpaSupplicant

getWifiR :: Handler WifiList
getWifiR = WpaSupplicant.runWlan0 $ liftA2 WifiList WpaSupplicant.getCurrentNetwork WpaSupplicant.listNetworks

postWifiR :: Handler ()
postWifiR = handleS9ErrT $ do
    AddWifiReq { addWifiSsid, addWifiPassword, addWifiCountry, skipConnect } <- requireCheckJsonBody
    unless (T.all isAscii addWifiSsid) $ throwE InvalidSsidE
    unless (T.all isAscii addWifiPassword) $ throwE InvalidPskE

    _ <- liftIO . forkIO . WpaSupplicant.runWlan0 $ do
        lift $ withAgentVersionLog_ [i|Adding new WiFi network: '#{addWifiSsid}'|]
        WpaSupplicant.addNetwork addWifiSsid addWifiPassword addWifiCountry
        unless skipConnect $ do
            mCurrent <- WpaSupplicant.getCurrentNetwork
            connected <- WpaSupplicant.selectNetwork addWifiSsid addWifiCountry
            unless connected do
                lift $ withAgentVersionLog_ [i|Failed to add new WiFi network: '#{addWifiSsid}'|]
                WpaSupplicant.removeNetwork addWifiSsid
                case mCurrent of
                    Nothing -> pure ()
                    Just current -> void $ WpaSupplicant.selectNetwork current addWifiSsid
    sendResponseStatus status200 ()


postWifiBySsidR :: Text -> Handler ()
postWifiBySsidR ssid = handleS9ErrT $ do
    unless (T.all isAscii ssid) $ throwE InvalidSsidE

    -- TODO: Front end never sends this on switching between networks. This means that we can only
    -- switch to US networks.
    country <- fromMaybe "US" <$> lookupGetParam "country"
    _       <- liftIO . forkIO . WpaSupplicant.runWlan0 $ do
        mCurrent <- WpaSupplicant.getCurrentNetwork
        connected <- WpaSupplicant.selectNetwork ssid country
        if connected
            then lift $ withAgentVersionLog_ [i|Successfully connected to WiFi: #{ssid}|]
            else do
                lift $ withAgentVersionLog_ [i|Failed to add new WiFi network: '#{ssid}'|]
                case mCurrent of
                    Nothing -> lift $ withAgentVersionLog_ [i|No WiFi to revert to!|]
                    Just current -> void $ WpaSupplicant.selectNetwork current country
    sendResponseStatus status200 ()

deleteWifiBySsidR :: Text -> Handler ()
deleteWifiBySsidR ssid = handleS9ErrT $ do
    unless (T.all isAscii ssid) $ throwE InvalidSsidE
    WpaSupplicant.runWlan0 $ do
        current <- WpaSupplicant.getCurrentNetwork
        case current of
            Nothing -> deleteIt
            Just ssid' -> if ssid == ssid'
                then do
                    eth0 <- WpaSupplicant.isConnectedToEthernet
                    if eth0
                        then deleteIt
                        else lift $ throwE WifiOrphaningE
                else deleteIt
    where deleteIt = void $ WpaSupplicant.removeNetwork ssid
