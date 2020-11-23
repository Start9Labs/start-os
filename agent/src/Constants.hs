module Constants where

import           Startlude

import           Data.Version                   ( showVersion )
import           Lib.Types.Emver                ( Version )
import           Paths_ambassador_agent         ( version )

agentVersion :: Version
agentVersion = fromString $ showVersion version

withAgentVersionLog :: (Show a, MonadIO m) => Text -> a -> m ()
withAgentVersionLog t a = liftIO $ putStrLn @Text $ show agentVersion <> "-- " <> t <> ": " <> show a

withAgentVersionLog_ :: Text -> IO ()
withAgentVersionLog_ t = putStrLn @Text $ show agentVersion <> "-- " <> t
