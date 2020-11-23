{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.External.Metrics.Df where

import           Startlude

import           System.Process

import           Lib.Error
import           Lib.External.Metrics.Types

-- Disk :: Size Used Avail Use%
data DfMetrics = DfMetrics
    { metricDiskSize           :: Maybe Gigabytes
    , metricDiskUsed           :: Maybe Gigabytes
    , metricDiskAvailable      :: Maybe Gigabytes
    , metricDiskUsedPercentage :: Maybe Percentage
    } deriving (Eq, Show)

getDfMetrics :: MonadIO m => S9ErrT m DfMetrics
getDfMetrics = fmap parseDf runDf

runDf :: MonadIO m => S9ErrT m Text
runDf = do
    (_, output, err') <- liftIO $ readProcessWithExitCode "df" ["-a", "/"] ""
    unless (null err') $ throwE . MetricE $ "df command failed with " <> toS err'
    pure $ toS output

parseDf :: Text -> DfMetrics
parseDf t =
    let dataLine                 = words <$> lines t `atMay` 1
        metricDiskSize           = fmap oneKBlocksToGigs . readMaybe =<< (`atMay` 1) =<< dataLine
        metricDiskUsed           = fmap oneKBlocksToGigs . readMaybe =<< (`atMay` 2) =<< dataLine
        metricDiskAvailable      = fmap oneKBlocksToGigs . readMaybe =<< (`atMay` 3) =<< dataLine
        metricDiskUsedPercentage = readMaybe =<< (`atMay` 4) =<< dataLine
    in  DfMetrics { .. }

oneKBlocksToGigs :: Double -> Gigabytes
oneKBlocksToGigs s = Gigabytes $ s / 1e6
