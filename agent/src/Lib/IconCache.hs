{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib.IconCache where

import           Startlude               hiding ( ask
                                                , catch
                                                , throwIO
                                                , Reader
                                                )

import           Conduit
import           Control.Concurrent.STM.TVar
import           Control.Effect.Reader.Labelled
import           Crypto.Hash
import qualified Data.Conduit.Binary           as CB
import qualified Data.HashMap.Strict           as HM
import           Data.String.Interpolate.IsString
import           Network.HTTP.Simple
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           UnliftIO.Exception

import           Lib.Error
import           Lib.SystemPaths         hiding ( (</>) )
import           Lib.Types.Core
import           Database.Persist.Sql           ( runSqlPool
                                                , repsert
                                                , ConnectionPool
                                                , delete
                                                )
import           Model
import           Control.Effect.Error
import           Crypto.Hash.Conduit            ( hashFile )
import           Util.File                      ( removeFileIfExists )

type HasIconTags sig m = HasLabelled "iconTagCache" (Reader (TVar (HM.HashMap AppId (Digest MD5)))) sig m

findIcon :: (HasFilesystemBase sig m, MonadIO m) => AppId -> m (Maybe FilePath)
findIcon appId = do
    bp    <- toS <$> getAbsoluteLocationFor iconBasePath
    icons <- liftIO $ (listDirectory bp) `catch` \(e :: IOException) ->
        if isDoesNotExistError e then createDirectoryIfMissing True bp *> pure [] else throwIO e
    pure $ (bp </>) <$> find ((show appId ==) . takeBaseName) icons

saveIcon :: ( HasFilesystemBase sig m
            , HasIconTags sig m
            , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
            , Has (Error S9Error) sig m
            , MonadIO m
            )
         => String
         -> m ()
saveIcon url = do
    bp  <- toS <$> getAbsoluteLocationFor iconBasePath
    req <- case parseRequest url of
        Nothing -> throwError $ RegistryParseE (toS url) "invalid url"
        Just x  -> pure x
    let saveAction = runConduit $ httpSource req getResponseBody .| CB.sinkFileCautious (bp </> takeFileName url)
    liftIO $ runResourceT $ saveAction `catch` \(e :: IOException) -> if isDoesNotExistError e
        then do
            liftIO $ createDirectoryIfMissing True bp
            saveAction
        else throwIO e
    tag <- hashFile (bp </> takeFileName url)
    saveTag (AppId . toS $ takeFileName url) tag

saveTag :: (HasIconTags sig m, HasLabelled "databaseConnection" (Reader ConnectionPool) sig m, MonadIO m)
        => AppId
        -> Digest MD5
        -> m ()
saveTag appId tag = do
    cache <- ask @"iconTagCache"
    pool  <- ask @"databaseConnection"
    liftIO $ runSqlPool (repsert (IconDigestKey appId) (IconDigest tag)) pool `catch` \(e :: SomeException) ->
        putStrLn @Text [i|Icon Cache Insertion Failed!: #{appId}, #{tag}, #{e}|]
    liftIO $ atomically $ modifyTVar cache $ HM.insert appId tag

clearIcon :: ( MonadIO m
             , HasLabelled "iconTagCache" (Reader (TVar (HM.HashMap AppId v0))) sig m
             , HasLabelled "databaseConnection" (Reader ConnectionPool) sig m
             , HasLabelled "filesystemBase" (Reader Text) sig m
             )
          => AppId
          -> m ()
clearIcon appId = do
    db       <- ask @"databaseConnection"
    iconTags <- ask @"iconTagCache"
    liftIO . atomically $ modifyTVar iconTags (HM.delete appId)
    liftIO $ runSqlPool (delete (IconDigestKey appId)) db
    findIcon appId >>= \case
        Nothing -> pure ()
        Just x  -> removeFileIfExists x
