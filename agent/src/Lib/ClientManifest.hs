{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib.ClientManifest where

import           Startlude               hiding ( takeWhile
                                                , toList
                                                )
import qualified Protolude.Base                as P

import           Control.Error.Util
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.HashMap.Strict
import qualified Data.Map.Strict               as Map
                                                ( toList )
import           Data.Singletons.TypeLits
import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Exinst
import           Network.Mime
import           Numeric.Natural
import           Streaming.Prelude             as Stream
                                         hiding ( show
                                                , for
                                                , toList
                                                , cons
                                                )
import           System.IO                      ( hClose )

import           Lib.Error
import           Lib.SystemPaths
import           Lib.Types.NetAddress
import           Lib.Types.Core
import           Lib.Types.Emver

data ClientManifest (n :: Nat) where
    V0 ::ClientManifestV0 -> ClientManifest 0

deriving instance Show (ClientManifest a)

instance Dict1 Show ClientManifest where
    dict1 sn = case sn of
        SNat -> Dict

data ClientManifestV0 = ClientManifestV0
    { clientManifestV0AppId       :: AppId
    , clientManifestV0AppVersion  :: Version
    , clientManifestV0Main        :: SystemPath
    , clientManifestV0UriRewrites :: HashMap UriPattern LanExp
    , clientManifestV0ErrorFiles  :: HashMap Int FilePath
    , clientManifestV0MimeRules   :: MimeMap
    , clientManifestV0MimeDefault :: MimeType
    }
    deriving Show

data UriPattern = MatchExact Text | MatchPrefix Text
    deriving (Eq, Show, Generic, Hashable)
newtype LanExp = LanExp { unLanExp :: (AppId, LanIp -> Text) }
instance Show LanExp where
    show (LanExp (AppId appId, f)) = toS . f . LanIp $ "{{" <> appId <> "}}"

parseUriPattern :: Parser UriPattern
parseUriPattern = do
    cons <- char '=' *> pure MatchExact <|> pure MatchPrefix
    cons . toS <$> takeWhile1 (not . isSpace)

parseUriRewrite :: Parser (UriPattern, LanExp)
parseUriRewrite = do
    pat <- parseUriPattern
    skipSpace
    void $ char '-' *> char '>'
    skipSpace
    tgt <- parseUriTarget
    pure (pat, tgt)

parseUriTarget :: Parser LanExp
parseUriTarget = do
    proto  <- (string "https" <|> string "http")
    opener <- string "://" <* string "{{"
    host   <- takeWhile1 (not . (== '}'))
    closer <- string "}}" *> string ":"
    port   <- decimal @Word16
    path   <- takeWhile1 (not . isSpace)
    pure . LanExp $ (AppId host, \ip -> proto <> opener <> unLanIp ip <> closer <> show port <> path)

instance FromJSON (Some1 ClientManifest) where
    parseJSON = withObject "Client Manifest" $ \o -> do
        v <- o .: "manifest-version"
        case (v :: Natural) of
            0 -> some1 . V0 <$> parseJSON (Object o)
            _ -> fail $ "Unsupported Manifest Version: " <> show v

instance FromJSON ClientManifestV0 where
    parseJSON = withObject "Client Manifest V0" $ \o -> do
        clientManifestV0AppId       <- o .: "app-id"
        clientManifestV0AppVersion  <- o .: "app-version"
        clientManifestV0Main        <- relBase <$> o .: "main-is"
        clientManifestV0UriRewrites <- fmap fromList $ o .: "uri-rewrites" >>= \rewrites -> do
            for (fmap (parseOnly parseUriRewrite) rewrites) $ \case
                Right r -> pure r
                Left  e -> fail $ "Invalid Rewrite Rule: " <> e
        clientManifestV0ErrorFiles  <- fromMaybe mempty <$> o .: "error-pages"
        clientManifestV0MimeRules   <- encodeUtf8 <<$>> o .: "mime-types"
        clientManifestV0MimeDefault <- encodeUtf8 <$> o .: "mime-default"
        pure ClientManifestV0 { .. }

testClientManifest :: ByteString
testClientManifest = [i|
manifest-version: 0
app-id: start9-ambassador
app-version: 0.2.0
main-is: /index.html
uri-rewrites:
    - =/api -> http://{{start9-ambassador}}:5959/authenticate
    - /api -> http://{{start9-ambassador}}:5959/
error-pages:
    404: /err404.html
mime-types:
    bin: application/octet-stream
    json: application/json
mime-default: text/plain
|]

data NginxSiteConf = NginxSiteConf
    { nginxSiteConfAppId        :: AppId
    , nginxSiteConfAppVersion   :: Version
    , nginxSiteConfRoot         :: SystemPath
    , nginxSiteConfListen       :: Word16
    , nginxSiteConfServerName   :: [Text]
    , nginxSiteConfLocations    :: [NginxLocation]
    , nginxSiteConfIndex        :: SystemPath
    , nginxSiteConfMimeMappings :: HashMap MimeType [Extension]
    , nginxSiteConfErrorPages   :: HashMap Int SystemPath
    , nginxSiteConfDefaultMime  :: MimeType
    , nginxSiteConfSsl          :: Maybe NginxSsl
    }
    deriving Show

data NginxLocation = NginxLocation
    { nginxLocationPattern :: UriPattern
    , nginxLocationTarget  :: Text
    }
    deriving Show

data NginxSsl = NginxSsl
    { nginxSslKeyPath         :: SystemPath
    , nginxSslCertPath        :: SystemPath
    , nginxSslOnlyServerNames :: [Text]
    }
    deriving Show

transpileV0ToNginx :: MonadReader (HashMap AppId (TorAddress, LanIp)) m => ClientManifest 0 -> S9ErrT m NginxSiteConf
transpileV0ToNginx (V0 ClientManifestV0 {..}) = do
    hm <- ask
    let nginxSiteConfAppId      = clientManifestV0AppId
    let nginxSiteConfAppVersion = clientManifestV0AppVersion
    let nginxSiteConfRoot = "/var/www/html" <> relBase (unAppId clientManifestV0AppId)
    let nginxSiteConfListen     = 80
    nginxSiteConfServerName <-
        pure . unTorAddress . fst <$> lookup clientManifestV0AppId hm ?? (EnvironmentValE clientManifestV0AppId)
    nginxSiteConfLocations <- for (toList clientManifestV0UriRewrites) $ \(pat, (LanExp (appId, tgt))) -> do
        lan <- snd <$> lookup appId hm ?? EnvironmentValE appId
        pure $ NginxLocation pat (tgt lan)
    let nginxSiteConfIndex      = clientManifestV0Main
    let nginxSiteConfErrorPages = fmap fromString clientManifestV0ErrorFiles
    let nginxSiteConfMimeMappings =
            flip execState Data.HashMap.Strict.empty $ for (Map.toList clientManifestV0MimeRules) $ \(ext, mime) -> do
                modify (alter (maybe (Just [ext]) (Just . (ext :))) mime)
    let nginxSiteConfDefaultMime = clientManifestV0MimeDefault
    let nginxSiteConfSsl         = Nothing
    pure NginxSiteConf { .. }

-- TODO WRONG, this caching disabled for all uri rewrites
-- this hack is ok for ambassador-ui, but does not generalize
-- we might want to deprecate this means of cachine anyway though
-- see: https://developers.google.com/web/ilt/pwa/caching-files-with-service-worker#cache_then_network
nginxConfGen :: MonadState Int m => NginxSiteConf -> Stream (Of Text) m ()
nginxConfGen NginxSiteConf {..} = do
    emit "server {"
    indent $ do
        emit $ "root " <> nginxSiteConfRoot `relativeTo` "/" <> ";"

        case nginxSiteConfSsl of
            Nothing -> emit $ "listen " <> show nginxSiteConfListen <> ";"
            Just _  -> emit $ "listen " <> show nginxSiteConfListen <> " ssl;"

        emit $ "server_name " <> (T.intercalate " " nginxSiteConfServerName) <> ";"

        case nginxSiteConfSsl of
            Nothing            -> pure ()
            Just NginxSsl {..} -> do
                emit $ "ssl_certificate " <> (nginxSslCertPath `relativeTo` "/") <> ";"
                emit $ "ssl_certificate_key " <> (nginxSslKeyPath `relativeTo` "/") <> ";"

        for_ nginxSiteConfLocations $ \(NginxLocation pat tgt) -> do
            case pat of
                MatchExact  p -> emit $ "location = " <> p <> " {"
                MatchPrefix p -> emit $ "location " <> p <> " {"
            indent $ do
                emit $ "proxy_pass " <> tgt <> ";"
                emit $ "proxy_set_header Host $host;"
            emit "}"
        emit "location = / {"
        indent $ do
            emit $ "add_header X-Consulate-App-ID " <> (show nginxSiteConfAppId) <> ";"
            emit $ "add_header X-Consulate-App-Version " <> (show nginxSiteConfAppVersion) <> ";"
            emit $ "add_header Cache-Control private;"
            emit $ "expires 86400;"
            emit $ "etag on;"
            emit $ "index " <> nginxSiteConfIndex `relativeTo` "/" <> ";"
        emit "}"
        for_ (toList nginxSiteConfErrorPages) $ \(ec, path) -> do
            emit $ "error_page " <> show ec <> " " <> (path `relativeTo` "/") <> ";"
            emit $ "location = " <> path `relativeTo` "/" <> " {"
            indent $ do
                emit $ "add_header X-Consulate-App-ID " <> (show nginxSiteConfAppId) <> ";"
                emit $ "add_header X-Consulate-App-Version " <> (show nginxSiteConfAppVersion) <> ";"
                emit "internal;"
            emit "}"
        emit "location / {"
        indent $ do
            emit $ "add_header X-Consulate-App-ID " <> (show nginxSiteConfAppId) <> ";"
            emit $ "add_header X-Consulate-App-Version " <> (show nginxSiteConfAppVersion) <> ";"
            emit $ "add_header Cache-Control private;"
            emit $ "expires 86400;"
            emit $ "etag on;"
        emit "}"
        emit "types {"
        indent $ for_ (toList nginxSiteConfMimeMappings) $ \(typ, exts) -> do
            emit $ decodeUtf8 typ <> " " <> T.unwords exts <> ";"
        emit "}"
        emit $ "default_type " <> decodeUtf8 nginxSiteConfDefaultMime <> ";"
    emit "}"
    case nginxSslOnlyServerNames <$> nginxSiteConfSsl of
        Nothing -> pure ()
        Just [] -> pure ()
        Just ls -> do
            emit "server {"
            indent $ do
                emit "listen 80;"
                emit $ "server_name " <> T.intercalate " " ls <> ";"
                emit $ "return 301 https://$host$request_uri;"
            emit "}"
    where
        emit :: MonadState Int m => Text -> Stream (Of Text) m ()
        emit t = get >>= \n -> yield $ T.replicate n "\t" <> t
        indent :: MonadState Int m => m a -> m a
        indent m = modify (+ (1 :: Int)) *> m <* modify (subtract (1 :: Int))

data NginxSiteConfOverride = NginxSiteConfOverride
    { nginxSiteConfOverrideAdditionalServerName :: Text
    , nginxSiteConfOverrideListen               :: Word16
    , nginxSiteConfOverrideSsl                  :: Maybe NginxSsl
    }
overrideNginx :: NginxSiteConfOverride -> NginxSiteConf -> NginxSiteConf
overrideNginx NginxSiteConfOverride {..} nginxSiteConf = nginxSiteConf
    { nginxSiteConfServerName = previousServerNames <> [nginxSiteConfOverrideAdditionalServerName]
    , nginxSiteConfListen     = nginxSiteConfOverrideListen
    , nginxSiteConfSsl        = nginxSiteConfOverrideSsl
    }
    where previousServerNames = nginxSiteConfServerName nginxSiteConf

-- takes if' app-manifest, converts it to an nginx conf, writes it to of'
transpile :: (MonadReader (HashMap AppId (TorAddress, LanIp)) m, MonadIO m)
          => Maybe NginxSiteConfOverride
          -> FilePath
          -> FilePath
          -> m Bool
transpile mOverride if' of' = do
    oh       <- liftIO $ openFile of' WriteMode
    hm       <- ask
    contents <- liftIO $ toS <$> Startlude.readFile if'
    case Yaml.decodeEither' (encodeUtf8 contents) :: Either Yaml.ParseException (Some1 ClientManifest) of
        Left e -> do
            Startlude.print e
            liftIO $ hClose oh
            pure False
        Right (Some1 _ cm) -> case cm of
            cmv0@(V0 _) -> case runExceptT (fmap overrides $ transpileV0ToNginx cmv0) hm of
                Left e -> do
                    Startlude.print e
                    liftIO $ hClose oh
                    pure False
                Right nsc -> do
                    flip (evalStateT @_ @Int) 0 $ Stream.toHandle oh $ Stream.toHandle stdout $ Stream.copy
                        (Stream.map toS $ nginxConfGen nsc)
                    liftIO $ hClose oh
                    pure True
    where
        overrides = case mOverride of
            Nothing -> id
            Just o  -> overrideNginx o

