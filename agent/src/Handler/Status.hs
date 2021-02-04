{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Status where

import           Startlude

import           Control.Carrier.Error.Either
import           Data.Aeson.Encoding
import           Git.Embed
import           Yesod.Core.Handler
import           Yesod.Core.Json
import           Yesod.Core.Types

import           Constants
import           Daemon.ZeroConf
import           Foundation
import           Handler.Types.Metrics
import           Handler.Types.V0.Specs
import           Handler.Types.V0.Base
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import           Lib.External.Metrics.Df
import qualified Lib.External.Registry         as Reg
import           Lib.External.Specs.CPU
import           Lib.External.Specs.Memory
import           Lib.Metrics
import           Lib.SystemPaths         hiding ( (</>) )
import           Lib.Tor
import           Settings
import           Control.Carrier.Lift           ( runM )
import           System.Process
import qualified UnliftIO
import           System.FileLock
import           Yesod.Core.Content             ( typePlain )
import           Conduit

getVersionR :: Handler AppVersionRes
getVersionR = pure . AppVersionRes $ agentVersion

getVersionLatestR :: Handler VersionLatestRes
getVersionLatestR = handleS9ErrT $ do
    s <- getsYesod appSettings
    v <- interp s $ Reg.getLatestAgentVersion
    pure $ VersionLatestRes v
    where interp s = ExceptT . liftIO . runError . injectFilesystemBaseFromContext s . runRegistryUrlIOC


getSpecsR :: Handler Encoding -- deprecated in 0.2.0
getSpecsR = handleS9ErrT $ do
    settings        <- getsYesod appSettings
    specsCPU        <- liftIO getCpuInfo
    specsMem        <- liftIO getMem
    specsDisk       <- fmap show . metricDiskSize <$> getDfMetrics
    specsNetworkId  <- lift . runM . injectFilesystemBaseFromContext settings $ getStart9AgentHostname
    specsTorAddress <- lift . runM . injectFilesystemBaseFromContext settings $ getAgentHiddenServiceUrl

    let specsAgentVersion = agentVersion
    returnJsonEncoding SpecsRes { .. }

getMetricsR :: Handler (JSONResponse MetricsRes)
getMetricsR = do
    app <- getYesod
    fmap (JSONResponse . MetricsRes) . handleS9ErrT . getServerMetrics $ app

embassyNamePath :: SystemPath
embassyNamePath = "/root/agent/name.txt"

patchServerR :: Handler ()
patchServerR = do
    PatchServerReq { patchServerReqName } <- requireCheckJsonBody @_ @PatchServerReq
    base <- getsYesod $ appFilesystemBase . appSettings
    liftIO $ writeFile (toS $ embassyNamePath `relativeTo` base) patchServerReqName

getGitR :: Handler Text
getGitR = pure $embedGitRevision

getLogsR :: Handler TypedContent
getLogsR = do
    let debugLock = "/root/agent/tmp/debug.lock"
    UnliftIO.bracket (liftIO $ lockFile debugLock Exclusive) (liftIO . unlockFile) $ const $ do
        liftIO $ callCommand "journalctl -u agent --since \"1 hour ago\" > /root/agent/tmp/debug.log"
        respondSource typePlain $ sourceFile "/root/agent/tmp/debug.log" .| awaitForever sendChunkBS
