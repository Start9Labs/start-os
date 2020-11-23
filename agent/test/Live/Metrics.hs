module Live.Metrics where

import           Lib.External.Metrics.Df
import           Lib.External.Metrics.Iotop
import           Lib.External.Metrics.ProcDev
import           Lib.External.Metrics.Top
import           Startlude

parseIotopOutput :: IO IotopMetrics
parseIotopOutput = parseIotop <$> readFile "./test/Live/iotop.sample"

parseTopOutput :: IO TopMetrics
parseTopOutput = parseTop <$> readFile "./test/Live/top.sample"

parseDfOutput :: IO DfMetrics
parseDfOutput = parseDf <$> readFile "./test/Live/df.sample"

parseProcDevOutput :: IO (UTCTime, ProcDevMomentStats)
parseProcDevOutput = do
    res <- readFile "./test/Live/procDev.sample"
    now <- getCurrentTime
    pure $ parseProcDev now res
