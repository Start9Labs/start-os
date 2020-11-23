{-# LANGUAGE RecordWildCards #-}

module Lib.Metrics where

import           Startlude

import           Data.Aeson
import           Data.IORef

import           Foundation
import           Lib.Error
import           Lib.External.Metrics.Df
import           Lib.External.Metrics.Iotop
import           Lib.External.Metrics.ProcDev
import           Lib.External.Metrics.Temperature
import           Lib.External.Metrics.Top
import           Lib.External.Metrics.Types

-- will throw only if one of '$ top', '$ iotop, '$ procDev' commands fails on the command line.
getServerMetrics :: MonadIO m => AgentCtx -> S9ErrT m ServerMetrics
getServerMetrics agentCtx = do
    temp            <- getTemperature
    df              <- getDfMetrics
    top             <- getTopMetrics
    iotop           <- getIotopMetrics
    (_, _, procDev) <- liftIO . readIORef . appProcDevMomentCache $ agentCtx

    pure $ fromCommandLineMetrics (temp, df, top, iotop, procDev)

data ServerMetrics = ServerMetrics
    { serverMetricsTemperature       :: Maybe Celsius

    , serverMetricMemPercentageUsed  :: Maybe Percentage
    , serverMetricMemFree            :: Maybe MebiBytes
    , serverMetricMemUsed            :: Maybe MebiBytes
    , serverMetricSwapTotal          :: Maybe MebiBytes
    , serverMetricSwapUsed           :: Maybe MebiBytes

    , serverMetricCpuIdle            :: Maybe Percentage
    , serverMetricCpuUserSpace       :: Maybe Percentage
    , serverMetricWait               :: Maybe Percentage
    , serverMetricCpuPercentageUsed  :: Maybe Percentage

    , serverMetricCurrentRead        :: Maybe BytesPerSecond
    , serverMetricCurrentWrite       :: Maybe BytesPerSecond
    , serverMetricTotalRead          :: Maybe BytesPerSecond
    , serverMetricTotalWrite         :: Maybe BytesPerSecond

    , serverMetricRBytesPerSecond    :: Maybe BytesPerSecond
    , serverMetricRPacketsPerSecond  :: Maybe BytesPerSecond
    , serverMetricRErrorsPerSecond   :: Maybe BytesPerSecond
    , serverMetricTBytesPerSecond    :: Maybe BytesPerSecond
    , serverMetricTPacketsPerSecond  :: Maybe BytesPerSecond
    , serverMetricTErrorsPerSecond   :: Maybe BytesPerSecond

    , serverMetricDiskSize           :: Maybe Gigabytes
    , serverMetricDiskUsed           :: Maybe Gigabytes
    , serverMetricDiskAvailable      :: Maybe Gigabytes
    , serverMetricDiskUsedPercentage :: Maybe Percentage
    } deriving (Eq, Show)

instance ToJSON ServerMetrics where
    toJSON ServerMetrics {..} = object
        [ "GENERAL" .= object ["Temperature" .= serverMetricsTemperature]
        , "MEMORY" .= object
            [ "Percent Used" .= serverMetricMemPercentageUsed
            , "Free" .= serverMetricMemFree
            , "Used" .= serverMetricMemUsed
            , "Swap Used" .= serverMetricSwapUsed
            , "Swap Free" .= serverMetricSwapTotal ?-? serverMetricSwapUsed
            ]
        , "CPU" .= object
            [ "Percent Used" .= serverMetricCpuPercentageUsed
            , "Percent Free" .= serverMetricCpuIdle
            , "Percent User Space" .= serverMetricCpuUserSpace
            , "Percent IO Wait" .= serverMetricWait
            ]
        , "DISK" .= object
            [ "Percent Used" .= serverMetricDiskUsedPercentage
            , "Size" .= serverMetricDiskSize
            , "Used" .= serverMetricDiskUsed
            , "Free" .= serverMetricDiskAvailable
            , "Total Read" .= serverMetricTotalRead
            , "Total Write" .= serverMetricTotalWrite
            , "Current Read" .= serverMetricCurrentRead
            , "Current Write" .= serverMetricCurrentWrite
            ]
        , "NETWORK" .= object
            [ "Bytes Received" .= serverMetricRBytesPerSecond
            , "Packets Received" .= serverMetricRPacketsPerSecond
            , "Errors Received" .= serverMetricRErrorsPerSecond
            , "Bytes Transmitted" .= serverMetricTBytesPerSecond
            , "Packets Transmitted" .= serverMetricTPacketsPerSecond
            , "Errors Transmitted" .= serverMetricTErrorsPerSecond
            ]
        ]
    toEncoding ServerMetrics {..} = (pairs . fold)
        [ "GENERAL" .= object ["Temperature" .= serverMetricsTemperature]
        , "MEMORY" .= object
            [ "Percent Used" .= serverMetricMemPercentageUsed
            , "Free" .= serverMetricMemFree
            , "Used" .= serverMetricMemUsed
            , "Swap Used" .= serverMetricSwapUsed
            , "Swap Free" .= serverMetricSwapTotal ?-? serverMetricSwapUsed
            ]
        , "CPU" .= object
            [ "Percent Used" .= serverMetricCpuPercentageUsed
            , "Percent Free" .= serverMetricCpuIdle
            , "Percent User Space" .= serverMetricCpuUserSpace
            , "Percent IO Wait" .= serverMetricWait
            ]
        , "DISK" .= object
            [ "Percent Used" .= serverMetricDiskUsedPercentage
            , "Size" .= serverMetricDiskSize
            , "Used" .= serverMetricDiskUsed
            , "Free" .= serverMetricDiskAvailable
            , "Total Read" .= serverMetricTotalRead
            , "Total Write" .= serverMetricTotalWrite
            , "Current Read" .= serverMetricCurrentRead
            , "Current Write" .= serverMetricCurrentWrite
            ]
        , "NETWORK" .= object
            [ "Bytes Received" .= serverMetricRBytesPerSecond
            , "Packets Received" .= serverMetricRPacketsPerSecond
            , "Errors Received" .= serverMetricRErrorsPerSecond
            , "Bytes Transmitted" .= serverMetricTBytesPerSecond
            , "Packets Transmitted" .= serverMetricTPacketsPerSecond
            , "Errors Transmitted" .= serverMetricTErrorsPerSecond
            ]
        ]

fromCommandLineMetrics :: (Maybe Celsius, DfMetrics, TopMetrics, IotopMetrics, ProcDevMetrics) -> ServerMetrics
fromCommandLineMetrics (temp, DfMetrics {..}, TopMetrics {..}, IotopMetrics {..}, ProcDevMetrics {..}) = ServerMetrics
    { serverMetricsTemperature       = temp
    , serverMetricMemPercentageUsed  = metricMemPercentageUsed
    , serverMetricMemFree            = metricMemFree
    , serverMetricMemUsed            = metricMemUsed
    , serverMetricSwapTotal          = metricSwapTotal
    , serverMetricSwapUsed           = metricSwapUsed
    , serverMetricCpuIdle            = metricCpuIdle
    , serverMetricCpuUserSpace       = metricCpuUserSpace
    , serverMetricWait               = metricWait
    , serverMetricCpuPercentageUsed  = metricCpuPercentageUsed
    , serverMetricCurrentRead        = metricCurrentRead
    , serverMetricCurrentWrite       = metricCurrentWrite
    , serverMetricTotalRead          = metricTotalRead
    , serverMetricTotalWrite         = metricTotalWrite
    , serverMetricRBytesPerSecond    = metricRBytesPerSecond
    , serverMetricRPacketsPerSecond  = metricRPacketsPerSecond
    , serverMetricRErrorsPerSecond   = metricRErrorsPerSecond
    , serverMetricTBytesPerSecond    = metricTBytesPerSecond
    , serverMetricTPacketsPerSecond  = metricTPacketsPerSecond
    , serverMetricTErrorsPerSecond   = metricTErrorsPerSecond
    , serverMetricDiskSize           = metricDiskSize
    , serverMetricDiskUsed           = metricDiskUsed
    , serverMetricDiskAvailable      = metricDiskAvailable
    , serverMetricDiskUsedPercentage = metricDiskUsedPercentage
    }
