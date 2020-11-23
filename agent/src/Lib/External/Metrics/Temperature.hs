module Lib.External.Metrics.Temperature where

import           Startlude

import qualified Data.Attoparsec.Text          as A
import qualified Data.Text                     as T
import           Lib.External.Metrics.Types
import           System.Process.Text

-- Pi4 Specific
getTemperature :: MonadIO m => m (Maybe Celsius)
getTemperature = liftIO $ do
    (ec, tempString, errlog) <- readProcessWithExitCode "/opt/vc/bin/vcgencmd" ["measure_temp"] ""
    unless (T.null errlog) $ putStrLn errlog
    case ec of
        ExitFailure _ -> pure Nothing
        ExitSuccess   -> case A.parse tempParser tempString of
            A.Done _ c -> pure $ Just c
            _          -> pure Nothing

tempParser :: A.Parser Celsius
tempParser = A.asciiCI "temp=" *> fmap Celsius A.double <* "'C" <* A.endOfLine
