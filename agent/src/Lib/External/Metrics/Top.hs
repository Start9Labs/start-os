{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.External.Metrics.Top where

import           Startlude

import qualified Data.HashMap.Strict           as HM
import           System.Process

import           Lib.Error
import           Lib.External.Metrics.Types
import           Lib.External.Util
import           Util.Text

data TopMetrics = TopMetrics
    { metricMemPercentageUsed :: Maybe Percentage
    , metricMemFree           :: Maybe MebiBytes
    , metricMemUsed           :: Maybe MebiBytes

    , metricSwapTotal         :: Maybe MebiBytes
    , metricSwapUsed          :: Maybe MebiBytes

    , metricCpuIdle           :: Maybe Percentage
    , metricCpuUserSpace      :: Maybe Percentage
    , metricWait              :: Maybe Percentage
    , metricCpuPercentageUsed :: Maybe Percentage
    } deriving (Eq, Show)

getTopMetrics :: MonadIO m => S9ErrT m TopMetrics
getTopMetrics = fmap parseTop runTop

runTop :: MonadIO m => S9ErrT m Text
runTop = do
    (_, output, err') <- liftIO $ readProcessWithExitCode "top" ["-bn1"] ""
    unless (null err') $ throwE . MetricE $ "top command failed with " <> toS err'
    pure $ toS output

parseTop :: Text -> TopMetrics
parseTop t = TopMetrics { metricMemPercentageUsed = getMemPercentageUsed <$> mem
                        , metricMemFree           = MebiBytes . memFree <$> mem
                        , metricMemUsed           = MebiBytes . memUsed <$> mem
                        , metricSwapTotal         = MebiBytes . memTotal <$> swapS
                        , metricSwapUsed          = MebiBytes . memUsed <$> swapS
                        , metricCpuIdle           = cpuId <$> cpu
                        , metricCpuUserSpace      = cpuUs <$> cpu
                        , metricWait              = cpuWa <$> cpu
                        , metricCpuPercentageUsed = getCpuPercentageUsed <$> cpu
                        }
    where
        topLines = lines t
        cpu      = getCpuAggregates topLines
        mem      = getMemAggregates memHeader topLines
        swapS    = getMemAggregates swapHeader topLines

memHeader :: Text
memHeader = "MiB Mem"

swapHeader :: Text
swapHeader = "MiB Swap"

data TopMemAggregates = TopMemAggregates
    { memTotal :: Double
    , memFree  :: Double
    , memUsed  :: Double
    } deriving (Eq, Show)

cpuHeader :: Text
cpuHeader = "%Cpu(s)"

data TopCpuAggregates = TopCpuAggregates
    { cpuUs :: Percentage
    , cpuSy :: Percentage
    , cpuNi :: Percentage
    , cpuId :: Percentage
    , cpuWa :: Percentage
    , cpuHi :: Percentage
    , cpuSi :: Percentage
    , cpuSt :: Percentage
    } deriving (Eq, Show)

getMemAggregates :: Text -> [Text] -> Maybe TopMemAggregates
getMemAggregates header topRes = do
    memLine <- getLineByHeader header topRes
    let stats = HM.fromList $ getStats readMaybe memLine
    memTotal <- HM.lookup "total" stats
    memFree  <- HM.lookup "free" stats
    memUsed  <- HM.lookup "used" stats
    pure TopMemAggregates { .. }

getCpuAggregates :: [Text] -> Maybe TopCpuAggregates
getCpuAggregates topRes = do
    memLine <- getLineByHeader cpuHeader topRes
    let stats = HM.fromList $ getStats (mkPercentage <=< readMaybe) memLine
    cpuUs <- HM.lookup "us" stats
    cpuSy <- HM.lookup "sy" stats
    cpuNi <- HM.lookup "ni" stats
    cpuId <- HM.lookup "id" stats
    cpuWa <- HM.lookup "wa" stats
    cpuHi <- HM.lookup "hi" stats
    cpuSi <- HM.lookup "si" stats
    cpuSt <- HM.lookup "st" stats
    pure TopCpuAggregates { .. }

getCpuPercentageUsed :: TopCpuAggregates -> Percentage
getCpuPercentageUsed TopCpuAggregates {..} = Percentage (100 - unPercent cpuId)

getMemPercentageUsed :: TopMemAggregates -> Percentage
getMemPercentageUsed TopMemAggregates {..} = Percentage . truncateTo @Double 10 . (* 100) $ memUsed / memTotal

getStats :: (Text -> Maybe a) -> Text -> [(Text, a)]
getStats parseData = mapMaybe (parseToPair parseData) . fmap (words . toS) . getMatches statRegex . toS
    where statRegex = "[0-9]+(.[0-9][0-9]?)? ([\x21-\x7E][^(,|.)]+)"
