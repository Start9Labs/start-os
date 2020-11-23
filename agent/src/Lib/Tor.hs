module Lib.Tor where

import           Startlude

import qualified Data.Text                     as T

import           Lib.SystemPaths

getAgentHiddenServiceUrl :: (HasFilesystemBase sig m, MonadIO m) => m Text
getAgentHiddenServiceUrl = T.strip <$> readSystemPath' agentTorHiddenServiceHostnamePath

getAgentHiddenServiceUrlMaybe :: (HasFilesystemBase sig m, MonadIO m) => m (Maybe Text)
getAgentHiddenServiceUrlMaybe = fmap T.strip <$> readSystemPath agentTorHiddenServiceHostnamePath
