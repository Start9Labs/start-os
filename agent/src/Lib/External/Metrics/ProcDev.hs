{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections  #-}

module Lib.External.Metrics.ProcDev where

import           Startlude

import           Lib.External.Util
import           Lib.External.Metrics.Types
import           Lib.Error
import           Util.Text

data ProcDevMetrics = ProcDevMetrics
    { metricRBytesPerSecond    :: Maybe BytesPerSecond
    , metricRPacketsPerSecond  :: Maybe BytesPerSecond
    , metricRErrorsPerSecond   :: Maybe BytesPerSecond
    , metricTBytesPerSecond    :: Maybe BytesPerSecond
    , metricTPacketsPerSecond  :: Maybe BytesPerSecond
    , metricTErrorsPerSecond   :: Maybe BytesPerSecond
    , metricFrom :: UTCTime -- time range across which the above rates were calculated
    , metricTo   :: UTCTime
    } deriving Show

getProcDevMetrics :: MonadIO m
                  => (UTCTime, ProcDevMomentStats)
                  -> S9ErrT m (UTCTime, ProcDevMomentStats, ProcDevMetrics)
getProcDevMetrics oldMomentStats = do
    newMomentStats@(newTime, newStats) <- newProcDevMomentStats
    let metrics = computeProcDevMetrics oldMomentStats newMomentStats
    pure (newTime, newStats, metrics)

newProcDevMomentStats :: MonadIO m => S9ErrT m (UTCTime, ProcDevMomentStats)
newProcDevMomentStats = do
    res <- runProcDev
    now <- liftIO getCurrentTime
    pure $ parseProcDev now res

runProcDev :: MonadIO m => S9ErrT m Text
runProcDev = do
    eOutput <- liftIO . try @SomeException $ readFile "/proc/net/dev"
    case eOutput of
        Left  e      -> throwE . MetricE $ "ProcDev proc file could not be read with " <> show e
        Right output -> pure . toS $ output

parseProcDev :: UTCTime -> Text -> (UTCTime, ProcDevMomentStats)
parseProcDev now t = do
    (now, ) . fold . foreach filteredLines $ \l ->
        let ws              = words l
            procDevRBytes   = ws `atMay` 1 >>= readMaybe
            procDevRPackets = ws `atMay` 2 >>= readMaybe
            procDevRErrors  = ws `atMay` 3 >>= readMaybe

            procDevTBytes   = ws `atMay` 9 >>= readMaybe
            procDevTPackets = ws `atMay` 10 >>= readMaybe
            procDevTErrors  = ws `atMay` 11 >>= readMaybe
        in  ProcDevMomentStats { .. }
    where
        wlanRegex     = "^[ ]{0,}wlan0"
        ethRegex      = "^[ ]{0,}eth0"

        isWlan        = containsMatch wlanRegex
        isEth         = containsMatch ethRegex

        filteredLines = filter (liftA2 (||) isWlan isEth) $ lines t

computeProcDevMetrics :: (UTCTime, ProcDevMomentStats) -> (UTCTime, ProcDevMomentStats) -> ProcDevMetrics
computeProcDevMetrics (fromTime, fromStats) (toTime, toStats) =
    let metricRBytesPerSecond   = getMetric (procDevRBytes fromStats, fromTime) (procDevRBytes toStats, toTime)
        metricRPacketsPerSecond = getMetric (procDevRPackets fromStats, fromTime) (procDevRPackets toStats, toTime)
        metricRErrorsPerSecond  = getMetric (procDevRErrors fromStats, fromTime) (procDevRErrors toStats, toTime)
        metricTBytesPerSecond   = getMetric (procDevTBytes fromStats, fromTime) (procDevTBytes toStats, toTime)
        metricTPacketsPerSecond = getMetric (procDevTPackets fromStats, fromTime) (procDevTPackets toStats, toTime)
        metricTErrorsPerSecond  = getMetric (procDevTErrors fromStats, fromTime) (procDevTErrors toStats, toTime)
        metricFrom              = fromTime
        metricTo                = toTime
    in  ProcDevMetrics { .. }

getMetric :: (Maybe Integer, UTCTime) -> (Maybe Integer, UTCTime) -> Maybe BytesPerSecond
getMetric (Just fromMetric, fromTime) (Just toMetric, toTime) = Just . BytesPerSecond $ if timeDiff == 0
    then 0
    else truncateTo @Double 10 . fromRational $ (fromIntegral $ toMetric - fromMetric) / (toRational timeDiff)
    where timeDiff = diffUTCTime toTime fromTime
getMetric _ _ = Nothing

data ProcDevMomentStats = ProcDevMomentStats
    { procDevRBytes    :: Maybe Integer
    , procDevRPackets  :: Maybe Integer
    , procDevRErrors   :: Maybe Integer
    , procDevTBytes    :: Maybe Integer
    , procDevTPackets  :: Maybe Integer
    , procDevTErrors   :: Maybe Integer
    } deriving (Eq, Show)

(?+?) :: Num a => Maybe a -> Maybe a -> Maybe a
(?+?) Nothing Nothing = Nothing
(?+?) m1      m2      = Just $ fromMaybe 0 m1 + fromMaybe 0 m2

(?-?) :: Num a => Maybe a -> Maybe a -> Maybe a
(?-?) Nothing Nothing = Nothing
(?-?) m1      m2      = Just $ fromMaybe 0 m1 - fromMaybe 0 m2

instance Semigroup ProcDevMomentStats where
    m1 <> m2 = ProcDevMomentStats rBytes rPackets rErrors tBytes tPackets tErrors
        where
            rBytes   = procDevRBytes m1 ?+? procDevRBytes m2
            rPackets = procDevRPackets m1 ?+? procDevRPackets m2
            rErrors  = procDevRErrors m1 ?+? procDevRErrors m2
            tBytes   = procDevTBytes m1 ?+? procDevTBytes m2
            tPackets = procDevTPackets m1 ?+? procDevTPackets m2
            tErrors  = procDevTErrors m1 ?+? procDevTErrors m2
instance Monoid ProcDevMomentStats where
    mempty = ProcDevMomentStats (Just 0) (Just 0) (Just 0) (Just 0) (Just 0) (Just 0)

getDefaultProcDevMetrics :: MonadIO m => m ProcDevMetrics
getDefaultProcDevMetrics = do
    now <- liftIO getCurrentTime
    pure $ ProcDevMetrics Nothing Nothing Nothing Nothing Nothing Nothing now now
