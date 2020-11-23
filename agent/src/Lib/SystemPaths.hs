{-# LANGUAGE ScopedTypeVariables #-}
module Lib.SystemPaths where

import           Startlude               hiding ( (<.>)
                                                , Reader
                                                , ask
                                                , runReader
                                                )

import           Control.Effect.Labelled        ( Labelled
                                                , runLabelled
                                                )
import           Control.Effect.Reader.Labelled
import           Data.List
import qualified Data.Text                     as T
import qualified Protolude.Base                as P
                                                ( show )
import           System.IO.Error                ( isDoesNotExistError )
import           System.Directory

import           Lib.Types.Core
import           Settings

strJoin :: Char -> Text -> Text -> Text
strJoin c a b = case (T.unsnoc a, T.uncons b) of
    (Nothing     , Nothing     ) -> ""
    (Nothing     , Just _      ) -> b
    (Just _      , Nothing     ) -> a
    (Just (_, c0), Just (c1, s)) -> case (c0 == c, c1 == c) of
        (True , True ) -> a <> s
        (False, False) -> a <> T.singleton c <> b
        _              -> a <> b

(</>) :: Text -> Text -> Text
(</>) = strJoin '/'

(<.>) :: Text -> Text -> Text
(<.>) = strJoin '.'

-- system paths behave the same as FilePaths mostly except that they can be rebased onto alternative roots so that things
-- can be tested in an isolated way. This uses a church encoding.
newtype SystemPath = SystemPath { relativeTo :: Text -> Text }
instance Eq SystemPath where
    (==) a b = a `relativeTo` "/" == b `relativeTo` "/"
instance Show SystemPath where
    show sp = P.show $ sp `relativeTo` "/"
instance Semigroup SystemPath where
    (SystemPath f) <> (SystemPath g) = SystemPath $ g . f
instance Monoid SystemPath where
    mempty = SystemPath id
instance IsString SystemPath where
    fromString (c : cs) = case c of
        '/' -> relBase . toS $ cs
        _   -> relBase . toS $ c : cs
    fromString [] = mempty

leaf :: SystemPath -> Text
leaf = last . T.splitOn "/" . show

relBase :: Text -> SystemPath
relBase = SystemPath . flip (</>)

type HasFilesystemBase sig m = HasLabelled "filesystemBase" (Reader Text) sig m

injectFilesystemBase :: Monad m => Text -> Labelled "filesystemBase" (ReaderT Text) m a -> m a
injectFilesystemBase fsbase = flip runReaderT fsbase . runLabelled @"filesystemBase"

injectFilesystemBaseFromContext :: Monad m => AppSettings -> Labelled "filesystemBase" (ReaderT Text) m a -> m a
injectFilesystemBaseFromContext = injectFilesystemBase . appFilesystemBase

getAbsoluteLocationFor :: HasFilesystemBase sig m => SystemPath -> m Text
getAbsoluteLocationFor path = do
    base <- ask @"filesystemBase"
    pure $ path `relativeTo` base

readSystemPath :: (HasFilesystemBase sig m, MonadIO m) => SystemPath -> m (Maybe Text)
readSystemPath path = do
    loadPath <- getAbsoluteLocationFor path
    contents <-
        liftIO
        $       (Just <$> readFile (toS loadPath))
        `catch` (\(e :: IOException) -> if isDoesNotExistError e then pure Nothing else throwIO e)
    pure contents

-- like the above, but throws IO error if file not found
readSystemPath' :: (HasFilesystemBase sig m, MonadIO m) => SystemPath -> m Text
readSystemPath' path = do
    loadPath <- getAbsoluteLocationFor path
    contents <- liftIO . readFile . toS $ loadPath
    pure contents

writeSystemPath :: (HasFilesystemBase sig m, MonadIO m) => SystemPath -> Text -> m ()
writeSystemPath path contents = do
    loadPath <- getAbsoluteLocationFor path
    liftIO $ writeFile (toS loadPath) contents

deleteSystemPath :: (HasFilesystemBase sig m, MonadIO m) => SystemPath -> m ()
deleteSystemPath path = do
    loadPath <- getAbsoluteLocationFor path
    liftIO $ removePathForcibly (toS loadPath)

dbPath :: (HasFilesystemBase sig m, HasLabelled "sqlDatabase" (Reader Text) sig m) => m Text
dbPath = do
    rt     <- ask @"filesystemBase"
    dbName <- ask @"sqlDatabase"
    pure $ rt </> "root/agent" </> toS dbName

uiPath :: SystemPath
uiPath = "/var/www/html"

agentDataDirectory :: SystemPath
agentDataDirectory = "/root/agent"

agentTmpDirectory :: SystemPath
agentTmpDirectory = "/root/agent/tmp"

bootConfigPath :: SystemPath
bootConfigPath = "/boot/config.txt"

bootConfigTempPath :: SystemPath
bootConfigTempPath = "/boot/config_tmp.txt"

executablePath :: SystemPath
executablePath = "/usr/local/bin"

-- Caches --

iconBasePath :: SystemPath
iconBasePath = "/root/agent/icons"

-- Nginx --

nginxConfig :: SystemPath
nginxConfig = "/etc/nginx/nginx.conf"

journaldConfig :: SystemPath
journaldConfig = "/etc/systemd/journald.conf"

nginxSitesAvailable :: SystemPath -> SystemPath
nginxSitesAvailable = ("/etc/nginx/sites-available" <>)

nginxSitesEnabled :: SystemPath -> SystemPath
nginxSitesEnabled = ("/etc/nginx/sites-enabled" <>)

nginxTorConf :: SystemPath
nginxTorConf = "/start9-ambassador.conf"

nginxSslConf :: SystemPath
nginxSslConf = "/start9-ambassador-ssl.conf"

-- SSH --

sshKeysDirectory :: SystemPath
sshKeysDirectory = "/home/pi/.ssh"

sshKeysFilePath :: SystemPath
sshKeysFilePath = sshKeysDirectory <> "authorized_keys"

-- Zero Conf --

avahiPath :: SystemPath
avahiPath = "/etc/avahi"

avahiServiceFolder :: SystemPath
avahiServiceFolder = avahiPath <> "services"

avahiServicePath :: Text -> SystemPath
avahiServicePath svc = avahiServiceFolder <> relBase (svc <.> "service")

-- Ambassador UI --

ambassadorUiPath :: SystemPath
ambassadorUiPath = uiPath <> "/start9-ambassador"

ambassadorUiManifestPath :: SystemPath
ambassadorUiManifestPath = ambassadorUiPath <> "/client-manifest.yaml"

-- Tor --

agentTorHiddenServiceDirectory :: SystemPath
agentTorHiddenServiceDirectory = "/var/lib/tor/agent"

agentTorHiddenServiceHostnamePath :: SystemPath
agentTorHiddenServiceHostnamePath = agentTorHiddenServiceDirectory <> "/hostname"

agentTorHiddenServicePrivateKeyPath :: SystemPath
agentTorHiddenServicePrivateKeyPath = agentTorHiddenServiceDirectory <> "/hs_ed25519_secret_key"

-- Server Config --

serverNamePath :: SystemPath
serverNamePath = "/root/agent/name.txt"

altRegistryUrlPath :: SystemPath
altRegistryUrlPath = "/root/agent/alt_registry_url.txt"

-- Session Auth Key --

sessionSigningKeyPath :: SystemPath
sessionSigningKeyPath = "/root/agent/start9.aes"

-- AppMgr --

appMgrRootPath :: SystemPath
appMgrRootPath = "/root/appmgr"

appMgrAppPath :: AppId -> SystemPath
appMgrAppPath = ((appMgrRootPath <> "apps") <>) . relBase . unAppId

lifelineBinaryPath :: SystemPath
lifelineBinaryPath = "/usr/local/bin/lifeline"

-- Open SSL --

rootCaDirectory :: SystemPath
rootCaDirectory = agentDataDirectory <> "/ca"

rootCaKeyPath :: SystemPath
rootCaKeyPath = rootCaDirectory <> "/private/embassy-root-ca.key.pem"

rootCaCertPath :: SystemPath
rootCaCertPath = rootCaDirectory <> "/certs/embassy-root-ca.cert.pem"

rootCaOpenSslConfPath :: SystemPath
rootCaOpenSslConfPath = rootCaDirectory <> "/openssl.conf"

intermediateCaDirectory :: SystemPath
intermediateCaDirectory = rootCaDirectory <> "/intermediate"

intermediateCaKeyPath :: SystemPath
intermediateCaKeyPath = intermediateCaDirectory <> "/private/embassy-int-ca.key.pem"

intermediateCaCertPath :: SystemPath
intermediateCaCertPath = intermediateCaDirectory <> "/certs/embassy-int-ca.crt.pem"

intermediateCaOpenSslConfPath :: SystemPath
intermediateCaOpenSslConfPath = intermediateCaDirectory <> "/openssl.conf"

sslDirectory :: SystemPath
sslDirectory = "/etc/nginx/ssl"

entityKeyPath :: Text -> SystemPath
entityKeyPath hostname = sslDirectory <> relBase ("/" <> hostname <> "-local.key.pem")

entityCertPath :: Text -> SystemPath
entityCertPath hostname = sslDirectory <> relBase ("/" <> hostname <> "-local.crt.pem")

entityConfPath :: Text -> SystemPath
entityConfPath hostname = sslDirectory <> relBase ("/" <> hostname <> "-local.conf")

-- Systemd

agentServicePath :: SystemPath
agentServicePath = "/etc/systemd/system/agent.service"
