module Util.Conduit where

import           Startlude

import           Conduit
import           Data.Text                     as T
import           Data.Attoparsec.Text

parseC :: MonadIO m => Parser b -> ConduitT Text b m ()
parseC parser = fix $ \cont -> parseWith g parser "" >>= \case
    Done rest result -> do
        yield result
        unless (T.null rest) $ leftover rest >> cont
    Fail _ _ msg -> panic $ toS msg
    Partial _    -> panic "INCOMPLETE PARSE"
    where
        g :: MonadIO m => ConduitT Text o m Text
        g = await >>= \case
            Nothing -> pure mempty
            Just x  -> print x >> pure x

lineParser :: Parser Text
lineParser = takeTill isEndOfLine <* endOfLine
