{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Lib.External.AppMgr where

import           Startlude               hiding ( hPutStrLn
                                                , toS
                                                )

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.HashMap.Strict           as HM
import           Data.String.Interpolate.IsString
import           Data.Text                      ( unpack )
import qualified Data.Yaml                     as Yaml
import           Exinst
import           Numeric.Natural
import           System.IO.Error
import           System.Process
import           System.Process.Typed    hiding ( createPipe )

import           Lib.Error
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.NetAddress
import           Lib.Types.Emver
import qualified Data.ByteString.Char8         as B8
import qualified Data.Attoparsec.Text          as Atto

readProcessWithExitCode' :: MonadIO m => String -> [String] -> ByteString -> m (ExitCode, ByteString, ByteString)
readProcessWithExitCode' a b c = liftIO $ do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setStderr byteStringOutput
                $ setEnvInherit
                $ setStdout byteStringOutput
                $ (System.Process.Typed.proc a b)
    withProcessWait pc $ \process -> atomically $ liftA3 (,,)
                                                         (waitExitCodeSTM process)
                                                         (fmap LBS.toStrict $ getStdout process)
                                                         (fmap LBS.toStrict $ getStderr process)

readProcessInheritStderr :: MonadIO m => String -> [String] -> ByteString -> m (ExitCode, ByteString)
readProcessInheritStderr a b c = liftIO $ do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setStderr inherit
                $ setEnvInherit
                $ setStdout byteStringOutput
                $ (System.Process.Typed.proc a b)
    withProcessWait pc
        $ \process -> atomically $ liftA2 (,) (waitExitCodeSTM process) (fmap LBS.toStrict $ getStdout process)

torRepair :: MonadIO m => m ExitCode
torRepair = liftIO $ system "appmgr tor repair"

getConfigurationAndSpec :: MonadIO m => AppId -> S9ErrT m Text
getConfigurationAndSpec appId = fmap decodeUtf8 $ do
    (ec, out) <- readProcessInheritStderr "appmgr" ["info", show appId, "-C", "--json"] ""
    case ec of
        ExitSuccess   -> pure out
        ExitFailure n -> throwE $ AppMgrE [i|info #{appId} -C \--json|] n

getAppMgrVersion :: MonadIO m => S9ErrT m Version
getAppMgrVersion = do
    (code, out) <- liftIO $ readProcessInheritStderr "appmgr" ["semver"] ""
    case code of
        ExitSuccess -> case hush $ Atto.parseOnly parseVersion $ decodeUtf8 out of
            Nothing -> throwE $ AppMgrParseE "semver" "" (B8.unpack out)
            Just av -> pure av
        ExitFailure n -> throwE $ AppMgrE "semver" n

installNewAppMgr :: MonadIO m => VersionRange -> S9ErrT m Version
installNewAppMgr avs = do
    getAppMgrVersion >>= \case
        Version (0, 1, 0, _) -> void $ readProcessInheritStderr "appmgr" ["self-update", "=0.1.1"] ""
        _                    -> pure ()
    (ec, _) <- readProcessInheritStderr "appmgr" ["self-update", show avs] ""
    case ec of
        ExitSuccess   -> getAppMgrVersion
        ExitFailure n -> throwE $ AppMgrE [i|self-update #{avs}|] n

torShow :: MonadIO m => AppId -> S9ErrT m (Maybe Text)
torShow appId = do
    (ec, out) <- liftIO $ readProcessInheritStderr "appmgr" ["tor", "show", show appId] ""
    case ec of
        ExitSuccess   -> pure $ Just (decodeUtf8 out)
        ExitFailure n -> case n of
            6  -> pure Nothing
            n' -> throwE $ AppMgrE "tor show" n'

getAppLogs :: MonadIO m => AppId -> m Text
getAppLogs appId = liftIO $ do
    (pipeRead, pipeWrite)    <- createPipe
    (_, _, _, handleProcess) <- createProcess (System.Process.proc "appmgr" ["logs", "--tail", "40", show appId])
        { std_out = UseHandle pipeWrite
        , std_err = UseHandle pipeWrite
        }
    void $ waitForProcess handleProcess
    content <- BS.hGetContents pipeRead
    pure $ decodeUtf8 content

notifications :: MonadIO m => AppId -> S9ErrT m [AppMgrNotif]
notifications appId = do
    (ec, bs) <- readProcessInheritStderr "appmgr" ["notifications", show appId, "--json"] ""
    case ec of
        ExitSuccess -> case eitherDecodeStrict bs of
            Left  e -> throwE $ AppMgrParseE "notifications" (decodeUtf8 bs) e
            Right x -> pure x
        ExitFailure n -> throwE $ AppMgrE [i|notifications #{appId} \--json|] n

stats :: MonadIO m => AppId -> S9ErrT m Text
stats appId = fmap decodeUtf8 $ do
    (ec, out) <- readProcessInheritStderr "appmgr" ["stats", show appId, "--json"] ""
    case ec of
        ExitSuccess   -> pure out
        ExitFailure n -> throwE $ AppMgrE [i|stats #{appId} \--json|] n

torReload :: MonadIO m => S9ErrT m ()
torReload = do
    (ec, _) <- readProcessInheritStderr "appmgr" ["tor", "reload"] ""
    case ec of
        ExitSuccess   -> pure ()
        ExitFailure n -> throwE $ AppMgrE "tor reload" n

diskShow :: MonadIO m => S9ErrT m [DiskInfo]
diskShow = do
    (ec, bs) <- readProcessInheritStderr "appmgr" ["disks", "show", "--json"] ""
    case ec of
        ExitSuccess -> case eitherDecodeStrict bs of
            Left  e -> throwE $ AppMgrParseE "disk info" (decodeUtf8 bs) e
            Right x -> pure x
        ExitFailure n -> throwE $ AppMgrE "disk show" n

backupCreate :: MonadIO m => Maybe Text -> AppId -> FilePath -> S9ErrT m ()
backupCreate password appId disk = do
    let args = case password of
            Nothing -> ["backup", "create", "-p", "\"\"", show appId, disk]
            Just p' -> ["backup", "create", "-p", unpack p', show appId, disk]
    (ec, _) <- readProcessInheritStderr "appmgr" args ""
    case ec of
        ExitFailure n | n < 0     -> throwE $ BackupE appId "Interrupted"
                      | n == 7    -> throwE $ BackupPassInvalidE
                      | otherwise -> throwE $ AppMgrE "backup" n
        ExitSuccess -> pure ()

backupRestore :: MonadIO m => Maybe Text -> AppId -> FilePath -> S9ErrT m ()
backupRestore password appId disk = do
    let args = case password of
            Nothing -> ["backup", "restore", "-p", "\"\"", show appId, disk]
            Just p' -> ["backup", "restore", "-p", unpack p', show appId, disk]
    (ec, _) <- readProcessInheritStderr "appmgr" args ""
    case ec of
        ExitFailure n | n < 0     -> throwE $ BackupE appId "Interrupted"
                      | n == 7    -> throwE $ BackupPassInvalidE
                      | otherwise -> throwE $ AppMgrE "backup" n
        ExitSuccess -> pure ()

data AppMgrLevel =
      INFO
    | SUCCESS
    | WARN
    | ERROR
    deriving (Eq, Show, Read)

instance FromJSON AppMgrLevel where
    parseJSON = withText "Level" $ \t -> case readMaybe t of
        Nothing -> fail $ "Invalid Level: " <> unpack t
        Just x  -> pure x

data AppMgrNotif = AppMgrNotif
    { appMgrNotifTime    :: Rational
    , appMgrNotifLevel   :: AppMgrLevel
    , appMgrNotifCode    :: Natural
    , appMgrNotifTitle   :: Text
    , appMgrNotifMessage :: Text
    }
    deriving (Eq, Show)

instance FromJSON AppMgrNotif where
    parseJSON = withObject "appmgr notification res" $ \o -> do
        appMgrNotifTime    <- o .: "time"
        appMgrNotifLevel   <- o .: "level"
        appMgrNotifCode    <- o .: "code"
        appMgrNotifTitle   <- o .: "title"
        appMgrNotifMessage <- o .: "message"
        pure AppMgrNotif { .. }

type Manifest = Some1 ManifestStructure
data ManifestStructure (n :: Nat) where
    ManifestV0 ::{ manifestTitle :: Text
        } -> ManifestStructure 0

instance FromJSON (Some1 ManifestStructure) where
    parseJSON = withObject "app manifest" $ \o -> do
        o .: "compat" >>= \t -> case (t :: Text) of
            "v0"  -> some1 <$> parseJSON @(ManifestStructure 0) (Object o)
            other -> fail $ "Unknown Compat Version" <> unpack other

instance FromJSON (ManifestStructure 0) where
    parseJSON = withObject "manifest v0" $ \o -> do
        manifestTitle <- o .: "title"
        pure $ ManifestV0 { .. }

torrcBase :: SystemPath
torrcBase = "/root/appmgr/tor/torrc"

torServicesYaml :: SystemPath
torServicesYaml = "/root/appmgr/tor/services.yaml"

appMgrAppsDirectory :: SystemPath
appMgrAppsDirectory = "/root/appmgr/apps"

readLanIps :: (MonadReader Text m, MonadIO m) => S9ErrT m (HM.HashMap AppId LanIp)
readLanIps = do
    base     <- ask
    contents <-
        liftIO $ (Just <$> readFile (unpack $ torServicesYaml `relativeTo` base)) `catch` \(e :: IOException) ->
            if isDoesNotExistError e then pure Nothing else throwIO e
    case contents of
        Nothing        -> pure HM.empty
        Just contents' -> do
            val <- case Yaml.decodeEither' (encodeUtf8 contents') of
                Left  e -> throwE $ AppMgrParseE "lan ip" contents' (show e)
                Right a -> pure a
            case Yaml.parseEither parser val of
                Left  e -> throwE $ AppMgrParseE "lan ip" (show val) e
                Right a -> pure a
    where
        parser :: Value -> Yaml.Parser (HM.HashMap AppId LanIp)
        parser = withObject "Tor Services Yaml" $ \o -> do
            hm <- o .: "map"
            let (services, infos) = unzip $ HM.toList hm
            ips <- traverse ipParser infos
            pure . HM.fromList $ zip (AppId <$> services) ips
        ipParser :: Value -> Yaml.Parser LanIp
        ipParser = withObject "Service Info" $ \o -> do
            ip <- o .: "ip"
            pure $ LanIp ip

data DiskInfo = DiskInfo
    { diskInfoDescription :: Maybe Text
    , diskInfoSize        :: Text
    , diskInfoLogicalName :: FilePath
    , diskInfoPartitions  :: [PartitionInfo]
    }
    deriving (Eq, Show)
instance FromJSON DiskInfo where
    parseJSON = withObject "Disk Info" $ \o -> do
        diskInfoDescription <- o .: "description"
        diskInfoSize        <- o .: "size"
        diskInfoLogicalName <- o .: "logicalname"
        diskInfoPartitions  <- o .: "partitions"
        pure DiskInfo { .. }
instance ToJSON DiskInfo where
    toJSON DiskInfo {..} = object
        [ "description" .= diskInfoDescription
        , "size" .= diskInfoSize
        , "logicalname" .= diskInfoLogicalName
        , "partitions" .= diskInfoPartitions
        ]

data PartitionInfo = PartitionInfo
    { partitionInfoLogicalName :: FilePath
    , partitionInfoSize        :: Maybe Text
    , partitionInfoIsMounted   :: Bool
    , partitionInfoLabel       :: Maybe Text
    }
    deriving (Eq, Show)
instance FromJSON PartitionInfo where
    parseJSON = withObject "Partition Info" $ \o -> do
        partitionInfoLogicalName <- o .: "logicalname"
        partitionInfoSize        <- o .: "size"
        partitionInfoIsMounted   <- o .: "is-mounted"
        partitionInfoLabel       <- o .: "label"
        pure PartitionInfo { .. }
instance ToJSON PartitionInfo where
    toJSON PartitionInfo {..} = object
        [ "logicalname" .= partitionInfoLogicalName
        , "size" .= partitionInfoSize
        , "isMounted" .= partitionInfoIsMounted
        , "label" .= partitionInfoLabel
        ]
