{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Lib.Migration where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.FileEmbed
import           Data.Text                      ( split
                                                , splitOn
                                                , strip
                                                )
import           Database.Persist.Sql
import           Lib.Error
import           Lib.Types.Emver
import           Model
import           Startlude

ioMigrationDbVersion :: ConnectionPool -> Version -> Version -> IO ()
ioMigrationDbVersion dbConn sourceVersion targetVersion = do
    putStrLn @Text $ "Executing migrations from " <> show sourceVersion <> " to " <> show targetVersion
    runSqlPool (migrateDbVersions sourceVersion targetVersion & handleS9ErrNuclear) dbConn

getCurrentDbVersion :: MonadIO m => ReaderT SqlBackend m (Maybe Version)
getCurrentDbVersion =
    fmap (executedMigrationTgtVersion . entityVal) <$> selectFirst [] [Desc ExecutedMigrationCreatedAt]

getMigrations :: [MigrationFile]
getMigrations = mapMaybe toMigrationFile $(embedDir "./migrations")

migrateDbVersions :: MonadIO m => Version -> Version -> S9ErrT (ReaderT SqlBackend m) ()
migrateDbVersions sourceVersion targetVersion = case mkMigrationCollection sourceVersion targetVersion getMigrations of
    Just (MigrationCollection migrations) -> lift $ traverse executeMigration migrations $> ()
    Nothing ->
        throwE . PersistentE $ "No path of migrations from " <> show sourceVersion <> " to " <> show targetVersion

executeMigration :: MonadIO m => MigrationFile -> ReaderT SqlBackend m ()
executeMigration mf = migrateSql mf >> insertMigration mf $> ()

insertMigration :: MonadIO m => MigrationFile -> ReaderT SqlBackend m (Key ExecutedMigration)
insertMigration (MigrationFile source target _) = do
    now <- liftIO getCurrentTime
    fmap entityKey . insertEntity $ ExecutedMigration now now source target

migrateSql :: MonadIO m => MigrationFile -> ReaderT SqlBackend m ()
migrateSql MigrationFile { sqlContent } = do
    print sqlContent'
    traverse_ runIt sqlContent'
    where
        runIt       = liftA2 (*>) (liftIO . putStrLn) $ flip (rawSql @(Single Int)) [] . (<> ";") . strip
        sqlContent' = filter (/= "") . fmap strip . split (== ';') $ decodeUtf8 sqlContent

toMigrationFile :: (FilePath, ByteString) -> Maybe MigrationFile
toMigrationFile (fp, bs) = case splitOn "::" (toS fp) of
    [source, target] -> do
        sourceVersion <- parseMaybe parseJSON $ String source
        targetVersion <- parseMaybe parseJSON $ String target
        let sqlContent = bs
        pure MigrationFile { .. }
    _ -> Nothing

newtype MigrationCollection = MigrationCollection { unMigrations :: [MigrationFile] } deriving (Eq, Show)
mkMigrationCollection :: Version -> Version -> [MigrationFile] -> Maybe MigrationCollection
mkMigrationCollection source target migrations
    | null migrations
    = Nothing
    | source == target
    = Just $ MigrationCollection []
    | otherwise
    = let mNext = maximumByMay targetVersion $ filter
              (\m -> sourceVersion m == source && targetVersion m > source && targetVersion m <= target)
              migrations
      in  case mNext of
              Nothing -> Nothing
              Just nextMig ->
                  MigrationCollection
                      .   (nextMig :)
                      .   unMigrations
                      <$> mkMigrationCollection (targetVersion nextMig) target migrations
    where
        maximumByMay :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
        maximumByMay f as =
            let reducer x acc = case acc of
                    Nothing -> Just x
                    Just y  -> if f x > f y then Just x else Just y
            in  foldr reducer Nothing as

data MigrationFile = MigrationFile
    { sourceVersion :: Version
    , targetVersion :: Version
    , sqlContent    :: ByteString
    }
    deriving (Eq, Show)
