module Handler.Tor where

import           Startlude

import           Data.Aeson
import           Yesod.Core

import           Foundation
import           Lib.SystemPaths
import           Lib.Tor
import           Control.Carrier.Lift           ( runM )

newtype GetTorRes = GetTorRes { unGetTorRes :: Text }
instance ToJSON GetTorRes where
    toJSON a = object ["torAddress" .= unGetTorRes a]
instance ToContent GetTorRes where
    toContent = toContent . toJSON
instance ToTypedContent GetTorRes where
    toTypedContent = toTypedContent . toJSON

getTorAddressR :: Handler GetTorRes
getTorAddressR = do
    settings <- getsYesod appSettings
    runM $ GetTorRes <$> injectFilesystemBaseFromContext settings getAgentHiddenServiceUrl
