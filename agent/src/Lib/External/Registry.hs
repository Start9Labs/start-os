{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.External.Registry where

import           Startlude               hiding ( (<.>)
                                                , Reader
                                                , ask
                                                , runReader
                                                )
import           Startlude.ByteStream    hiding ( count )

import           Conduit
import           Control.Algebra
import           Control.Effect.Lift
import           Control.Effect.Error
import           Control.Effect.Reader.Labelled
import           Control.Monad.Fail             ( fail )
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Streaming.HTTP
                                               as S
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromJust )
import           Data.String.Interpolate.IsString
import           Data.Yaml
import           Network.HTTP.Client.Conduit    ( Manager )
import           Network.HTTP.Simple
import           System.Directory
import           System.Process

import           Constants
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.ServerApp
import           Data.Time.ISO8601              ( parseISO8601 )

newtype AppManifestRes = AppManifestRes
    { storeApps :: [StoreApp] } deriving (Eq, Show)

newtype RegistryVersionForSpecRes = RegistryVersionForSpecRes
    { registryVersionForSpec :: Maybe Version } deriving (Eq, Show)

instance FromJSON RegistryVersionForSpecRes where
    parseJSON Null       = pure (RegistryVersionForSpecRes Nothing)
    parseJSON (Object o) = do
        registryVersionForSpec <- o .:? "version"
        pure . RegistryVersionForSpecRes $ registryVersionForSpec
    parseJSON _ = fail "expected null or object"

tmpAgentFileName :: Text
tmpAgentFileName = "agent-tmp"

agentFileName :: Text
agentFileName = "agent"

userAgentHeader :: ByteString
userAgentHeader = [i|EmbassyOS/#{agentVersion}|]

setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" [userAgentHeader]

getYoungAgentBinary :: (Has RegistryUrl sig m, HasLabelled "filesystemBase" (Reader Text) sig m, Has (Lift IO) sig m)
                    => VersionRange
                    -> m ()
getYoungAgentBinary avs = do
    base <- ask @"filesystemBase"
    let tmpAgentPath = toS $ executablePath `relativeTo` base </> tmpAgentFileName
    tmpExists <- sendIO $ doesPathExist tmpAgentPath
    when tmpExists $ sendIO $ removeFile tmpAgentPath
    url     <- registryAppAgentUrl avs
    request <- sendIO . fmap setUserAgent . parseRequestThrow $ toS url
    sendIO $ runConduitRes $ httpSource request getResponseBody .| sinkFile tmpAgentPath
    sendIO $ void $ readProcessWithExitCode "chmod" ["700", tmpAgentPath] ""

getLifelineBinary :: (Has RegistryUrl sig m, HasFilesystemBase sig m, MonadIO m) => VersionRange -> m ()
getLifelineBinary avs = do
    base <- ask @"filesystemBase"
    let lifelineTarget = lifelineBinaryPath `relativeTo` base
    url     <- registryUrl
    request <- liftIO . fmap setUserAgent . parseRequestThrow $ toS (url </> "sys/lifeline?spec=" <> show avs)
    liftIO $ runConduitRes $ httpSource request getResponseBody .| sinkFile (toS lifelineTarget)
    liftIO $ void $ readProcessWithExitCode "chmod" ["700", toS lifelineTarget] ""

getAppManifest :: (MonadIO m, Has (Error S9Error) sig m, Has RegistryUrl sig m) => m AppManifestRes
getAppManifest = do
    manifestPath <- registryManifestUrl
    req          <- liftIO $ fmap setUserAgent . parseRequestThrow $ toS manifestPath
    val          <- (liftIO . try @SomeException) (httpBS req) >>= \case
        Left  _ -> throwError RegistryUnreachableE
        Right a -> pure $ getResponseBody a
    parseBsManifest val >>= \case
        Left  e -> throwError $ RegistryParseE manifestPath . toS $ e
        Right a -> pure a


getStoreAppInfo :: (MonadIO m, Has RegistryUrl sig m, Has (Error S9Error) sig m) => AppId -> m (Maybe StoreApp)
getStoreAppInfo name = find ((== name) . storeAppId) . storeApps <$> getAppManifest

parseBsManifest :: Has RegistryUrl sig m => ByteString -> m (Either String AppManifestRes)
parseBsManifest bs = do
    parseRegistryRes' <- parseRegistryRes
    pure $ parseEither parseRegistryRes' . fromJust . decodeThrow $ bs

parseRegistryRes :: Has RegistryUrl sig m => m (Value -> Parser AppManifestRes)
parseRegistryRes = do
    parseAppData' <- parseAppData
    pure $ withObject "app registry response" $ \obj -> do
        let keyVals       = HM.toList obj
        let mManifestApps = fmap (\(k, v) -> parseMaybe (parseAppData' (AppId k)) v) keyVals
        pure . AppManifestRes . catMaybes $ mManifestApps

registryUrl :: (Has RegistryUrl sig m) => m Text
registryUrl = maybe "https://registry.start9labs.com:443" show <$> getRegistryUrl

registryManifestUrl :: Has RegistryUrl sig m => m Text
registryManifestUrl = registryUrl <&> (</> "apps")

registryAppAgentUrl :: Has RegistryUrl sig m => VersionRange -> m Text
registryAppAgentUrl avs = registryUrl <&> (</> ("sys/agent?spec=" <> show avs))

registryCheckVersionForSpecUrl :: Has RegistryUrl sig m => VersionRange -> m Text
registryCheckVersionForSpecUrl avs = registryUrl <&> (</> ("sys/version/agent?spec=" <> show avs))

parseAppData :: Has RegistryUrl sig m => m (AppId -> Value -> Parser StoreApp)
parseAppData = do
    url <- registryUrl
    pure $ \storeAppId -> withObject "appmgr app data" $ \ad -> do
        storeAppTitle            <- ad .: "title"
        storeAppDescriptionShort <- ad .: "description" >>= (.: "short")
        storeAppDescriptionLong  <- ad .: "description" >>= (.: "long")
        storeAppIconUrl          <- fmap (\typ -> toS $ url </> "icons" </> show storeAppId <.> typ) $ ad .: "icon-type"
        storeAppVersions         <- ad .: "version-info" >>= \case
            []       -> fail "No Valid Version Info"
            (x : xs) -> pure $ x :| xs
        storeAppTimestamp <- ad .: "timestamp" >>= maybe (fail "Invalid ISO8601 Timestamp") pure . parseISO8601
        pure StoreApp { .. }

getAppVersionForSpec :: (Has RegistryUrl sig m, Has (Error S9Error) sig m, MonadIO m)
                     => AppId
                     -> VersionRange
                     -> m Version
getAppVersionForSpec appId spec = do
    let path = "apps/version" </> show appId <> "?spec=" <> show spec
    val <- registryRequest path
    parseOrThrow path val $ withObject "version response" $ \o -> do
        v <- o .: "version"
        pure v

getLatestAgentVersion :: (Has RegistryUrl sig m, Has (Error S9Error) sig m, MonadIO m) => m (Version, Maybe Text)
getLatestAgentVersion = do
    val <- registryRequest agentVersionPath
    parseOrThrow agentVersionPath val $ withObject "version response" $ \o -> do
        v  <- o .: "version"
        rn <- o .:? "release-notes"
        pure (v, rn)
    where agentVersionPath = "sys/version/agent"

getLatestAgentVersionForSpec :: (Has RegistryUrl sig m, Has (Lift IO) sig m, Has (Error S9Error) sig m)
                             => VersionRange
                             -> m (Maybe Version)
getLatestAgentVersionForSpec avs = do
    url <- registryUrl
    req <- sendIO $ fmap setUserAgent . parseRequestThrow . toS $ url </> agentVersionPath
    res <- fmap (first jsonToS9Exception) . sendIO $ try @JSONException $ parseRes req
    case res of
        Left  e -> throwError e
        Right a -> pure a
    where
        parseRes r = registryVersionForSpec . getResponseBody <$> httpJSON r
        agentVersionPath  = "sys/version/agent?spec=" <> show avs
        jsonToS9Exception = RegistryParseE (toS agentVersionPath) . show

getAmbassadorUiForSpec :: (Has RegistryUrl sig m, HasLabelled "httpManager" (Reader Manager) sig m, MonadResource m)
                       => VersionRange
                       -> ByteStream m ()
getAmbassadorUiForSpec avs = do
    url     <- lift registryUrl
    manager <- lift $ ask @"httpManager"
    let target = url </> "sys/ambassador-ui.tar.gz?spec=" <> show avs
    req  <- liftResourceT $ lift $ fmap setUserAgent . parseRequestThrow . toS $ target
    resp <- lift $ S.http req manager
    getResponseBody resp

registryRequest :: (Has RegistryUrl sig m, Has (Error S9Error) sig m, MonadIO m) => Text -> m Value
registryRequest path = do
    url <- registryUrl
    req <- liftIO . fmap setUserAgent . parseRequestThrow . toS $ url </> path
    (liftIO . try @SomeException) (httpJSON req) >>= \case
        Left  _ -> throwError RegistryUnreachableE
        Right a -> pure $ getResponseBody a

parseOrThrow :: (Has (Error S9Error) sig m) => Text -> a -> (a -> Parser b) -> m b
parseOrThrow path val parser = case parseEither parser val of
    Left  e -> throwError (RegistryParseE path $ toS e)
    Right a -> pure a
