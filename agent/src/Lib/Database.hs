module Lib.Database where

import           Startlude               hiding ( throwIO
                                                , Reader
                                                )

import           Control.Effect.Reader.Labelled
import           Control.Monad.Logger
import           Database.Persist.Sql
import           System.Directory

import           Constants
import           Lib.Migration
import           Lib.SystemPaths
import           Lib.Types.Emver
import           Model
import           Util.Function

------------------------------------------------------------------------------------------------------------------------
-- Migrations
------------------------------------------------------------------------------------------------------------------------

data UpMigrationHistory = UpMigrationHistory (Maybe Version) (Maybe Version) -- previous db version, current db version.

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

ensureCoherentDbVersion :: (HasFilesystemBase sig m, HasLabelled "sqlDatabase" (Reader Text) sig m, MonadIO m)
                        => ConnectionPool
                        -> Logger
                        -> m UpMigrationHistory
ensureCoherentDbVersion pool logFunc = do
    db         <- dbPath
    mDbVersion <- liftIO $ doesFileExist (toS db) >>= \case
        True  -> runSqlPool getCurrentDbVersion pool -- get db version if db exists
        False -> pure Nothing

    liftIO $ case mDbVersion of
        Nothing        -> initializeDb agentVersion pool logFunc
        Just dbVersion -> upMigration pool dbVersion agentVersion

initializeDb :: Version -> ConnectionPool -> Logger -> IO UpMigrationHistory
initializeDb av = runLoggingT .* runSqlPool $ do
    now <- liftIO getCurrentTime
    runMigration migrateAll
    void . insertEntity $ ExecutedMigration now now av av
    pure $ UpMigrationHistory Nothing (Just agentVersion)

upMigration :: ConnectionPool -> Version -> Version -> IO UpMigrationHistory
upMigration pool dbVersion currentAgentVersion = if dbVersion < currentAgentVersion
    then do
        ioMigrationDbVersion pool dbVersion currentAgentVersion
        pure $ UpMigrationHistory (Just dbVersion) (Just currentAgentVersion)
    else pure $ UpMigrationHistory (Just dbVersion) Nothing
