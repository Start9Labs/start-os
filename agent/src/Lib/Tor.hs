module Lib.Tor where

import           Startlude

import qualified Data.Text                     as T
import           Network.HTTP.Client
import           Network.Connection

import           Lib.SystemPaths
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import           Data.Default

getAgentHiddenServiceUrl :: (HasFilesystemBase sig m, MonadIO m) => m Text
getAgentHiddenServiceUrl = T.strip <$> readSystemPath' agentTorHiddenServiceHostnamePath

getAgentHiddenServiceUrlMaybe :: (HasFilesystemBase sig m, MonadIO m) => m (Maybe Text)
getAgentHiddenServiceUrlMaybe = fmap T.strip <$> readSystemPath agentTorHiddenServiceHostnamePath

-- | 'newTorManager' currently assumes the tor client lives on the localhost. The port comes in over an argument.
-- If this is insufficient in the future, feel free to parameterize the host.
newTorManager :: Word16 -> IO Manager
newTorManager = newManager . mkManagerSettings def . Just . SockSettingsSimple "127.0.0.1" . fromIntegral
