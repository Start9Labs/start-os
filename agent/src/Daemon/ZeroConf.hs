{-# LANGUAGE TypeApplications #-}
module Daemon.ZeroConf where

import           Startlude               hiding ( ask )

import           Control.Lens
import           Control.Effect.Reader.Labelled ( ask )
import           Control.Monad.Trans.Reader     ( withReaderT )
import           Crypto.Hash
import           Data.ByteArray                 ( convert )
import           Data.ByteArray.Encoding
import qualified Data.ByteString               as BS
import           System.FilePath.Lens

import           Foundation
import qualified Lib.Avahi                     as Avahi
import           Lib.ProductKey
import           Lib.SystemPaths

import           Settings

start9AgentServicePrefix :: IsString a => a
start9AgentServicePrefix = "start9-"

getStart9AgentHostname :: (HasFilesystemBase sig m, MonadIO m, ConvertText Text a) => m a
getStart9AgentHostname = do
    base   <- ask @"filesystemBase"
    suffix <-
        liftIO
        $   decodeUtf8
        .   convertToBase Base16
        .   BS.take 4
        .   convert
        .   hashWith SHA256
        .   encodeUtf8
        <$> getProductKey base
    pure . toS $ start9AgentServicePrefix <> suffix

getStart9AgentHostnameLocal :: (HasFilesystemBase sig m, MonadIO m) => m Text
getStart9AgentHostnameLocal = getStart9AgentHostname <&> (<> ".local")

publishAgentToAvahi :: ReaderT AgentCtx IO ()
publishAgentToAvahi = do
    filesystemBase     <- asks $ appFilesystemBase . appSettings
    start9AgentService <- injectFilesystemBase filesystemBase getStart9AgentHostname
    lift $ Avahi.createDaemonConf $ toS start9AgentService
    agentPort <- asks $ appPort . appSettings
    services  <- lift Avahi.listServices
    let serviceNames = view basename <$> services
    unless (start9AgentService `elem` serviceNames) $ withReaderT appSettings $ Avahi.createService
        (toS start9AgentService)
        (Avahi.WildcardsEnabled, "%h")
        "_http._tcp"
        agentPort
    lift Avahi.reload

