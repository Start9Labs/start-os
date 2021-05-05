{-# LANGUAGE PartialTypeSignatures #-}
module Handler.Icons where

import           Startlude               hiding ( Reader
                                                , runReader
                                                )

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Data.Conduit
import           Data.Conduit.Binary           as CB
import qualified Data.Text                     as T
import           Network.HTTP.Simple
import           System.FilePath.Posix
import           Yesod.Core

import           Control.Carrier.Reader  hiding ( asks )
import           Control.Concurrent.STM         ( modifyTVar
                                                , readTVarIO
                                                )
import           Control.Effect.Labelled        ( runLabelled )
import           Crypto.Hash.Conduit            ( hashFile )
import qualified Data.HashMap.Strict           as HM
import           Foundation
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import qualified Lib.External.Registry         as Reg
import           Lib.IconCache
import           Lib.SystemPaths         hiding ( (</>) )
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.ServerApp
import           Settings

iconUrl :: AppId -> Version -> Text
iconUrl appId version = (foldMap (T.cons '/') . fst . renderRoute . AppIconR $ appId) <> "?" <> show version

storeIconUrl :: AppId -> Version -> Text
storeIconUrl appId version =
    (foldMap (T.cons '/') . fst . renderRoute . AvailableAppIconR $ appId) <> "?" <> show version

getAppIconR :: AppId -> Handler TypedContent
getAppIconR appId = handleS9ErrT $ do
    ctx <- getYesod
    let iconTags = appIconTags ctx
    storedTag <- liftIO $ readTVarIO iconTags >>= pure . HM.lookup appId
    path      <- case storedTag of
        Nothing -> interp ctx $ do
            findIcon appId >>= \case
                Nothing -> fetchIcon
                Just fp -> do
                    tag <- hashFile fp
                    saveTag appId tag
                    pure fp
        Just x -> do
            setWeakEtag (show x)
            interp ctx $ findIcon appId >>= \case
                Nothing -> do
                    liftIO $ atomically $ modifyTVar iconTags (HM.delete appId)
                    fetchIcon
                Just fp -> pure fp
    cacheSeconds 86_400
    lift $ respondSource (parseContentType path) $ CB.sourceFile path .| awaitForever sendChunkBS
    where
        fetchIcon = do
            url <- find ((== appId) . storeAppId) . Reg.storeApps <$> Reg.getAppIndex >>= \case
                Nothing -> throwError $ NotFoundE "icon" (show appId)
                Just x  -> pure . toS $ storeAppIconUrl x
            bp <- getAbsoluteLocationFor iconBasePath
            saveIcon url
            pure (toS bp </> takeFileName url)
        interp ctx =
            mapExceptT (liftIO . runM)
                . runReader (appConnPool ctx)
                . runLabelled @"databaseConnection"
                . runReader (appFilesystemBase $ appSettings ctx)
                . runLabelled @"filesystemBase"
                . runReader (appIconTags ctx)
                . runLabelled @"iconTagCache"
                . runRegistryUrlIOC


getAvailableAppIconR :: AppId -> Handler TypedContent
getAvailableAppIconR appId = handleS9ErrT $ do
    s   <- getsYesod appSettings
    url <- do
        find ((== appId) . storeAppId) . Reg.storeApps <$> interp s Reg.getAppIndex >>= \case
            Nothing -> throwE $ NotFoundE "icon" (show appId)
            Just x  -> pure . toS $ storeAppIconUrl x
    req <- case parseRequest url of
        Nothing -> throwE $ RegistryParseE (toS url) "invalid url"
        Just x  -> pure x
    cacheSeconds 86_400
    lift $ respondSource (parseContentType url) $ httpSource req getResponseBody .| awaitForever sendChunkBS
    where interp s = ExceptT . liftIO . runError . injectFilesystemBaseFromContext s . runRegistryUrlIOC

parseContentType :: FilePath -> ContentType
parseContentType = contentTypeMapping . takeExtension
    where
        contentTypeMapping ext = case ext of
            ".png"  -> typePng
            ".jpeg" -> typeJpeg
            ".jpg"  -> typeJpeg
            ".gif"  -> typeGif
            ".svg"  -> typeSvg
            _       -> typePlain
