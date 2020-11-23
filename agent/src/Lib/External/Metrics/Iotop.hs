{-# LANGUAGE FlexibleContexts #-}

module Lib.External.Metrics.Iotop where

import           Startlude

import qualified Data.HashMap.Strict           as HM
import           System.Process

import           Lib.Error
import           Lib.External.Metrics.Types
import           Lib.External.Util
import           Util.Text

data IotopMetrics = IotopMetrics
    { metricCurrentRead  :: Maybe BytesPerSecond
    , metricCurrentWrite :: Maybe BytesPerSecond
    , metricTotalRead    :: Maybe BytesPerSecond
    , metricTotalWrite   :: Maybe BytesPerSecond
    } deriving (Eq, Show)

getIotopMetrics :: MonadIO m => S9ErrT m IotopMetrics
getIotopMetrics = fmap parseIotop runIotop

runIotop :: MonadIO m => S9ErrT m Text
runIotop = do
    (_, output, err') <- liftIO $ readProcessWithExitCode "iotop" ["-bn1"] ""
    unless (null err') $ throwE . MetricE $ "iotop command failed with " <> toS err'
    pure $ toS output

parseIotop :: Text -> IotopMetrics
parseIotop t = IotopMetrics { metricCurrentRead  = BytesPerSecond . fst <$> current
                            , metricCurrentWrite = BytesPerSecond . snd <$> current
                            , metricTotalRead    = BytesPerSecond . fst <$> total
                            , metricTotalWrite   = BytesPerSecond . snd <$> total
                            }
    where
        iotopLines = lines t
        current    = getHeaderAggregates currentHeader iotopLines
        total      = getHeaderAggregates totalHeader iotopLines

currentHeader :: Text
currentHeader = "Current"

totalHeader :: Text
totalHeader = "Total"

getHeaderAggregates :: Text -> [Text] -> Maybe (Double, Double)
getHeaderAggregates header iotopLines = do
    actualLine <- getLineByHeader header iotopLines
    let stats = HM.fromList . getStats $ actualLine
    r <- HM.lookup "READ" stats
    w <- HM.lookup "WRITE" stats
    pure (r, w)
getStats :: Text -> [(Text, Double)]
getStats = mapMaybe (parseToPair readMaybe . words . gsub ":" "") . getMatches statRegex
    where statRegex = "([\x21-\x7E]+)[ ]{0,}:[ ]{1,}([\x21-\x7E]+)"

