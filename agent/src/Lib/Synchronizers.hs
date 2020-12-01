{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
module Lib.Synchronizers where

import           Startlude               hiding ( check
                                                , err
                                                )
import qualified Startlude.ByteStream          as ByteStream
import qualified Startlude.ByteStream.Char8    as ByteStream

import qualified Control.Effect.Reader.Labelled
                                               as Fused
import           Control.Carrier.Lift           ( runM )
import           Control.Monad.Trans.Reader     ( mapReaderT )
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B8
import qualified Data.Conduit                  as Conduit
import qualified Data.Conduit.Combinators      as Conduit
import qualified Data.Conduit.Tar              as Conduit
import           Data.Conduit.Shell      hiding ( arch
                                                , patch
                                                , stream
                                                , hostname
                                                )
import           Data.FileEmbed
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.String.Interpolate.IsString
import qualified Data.Yaml                     as Yaml
import           Exinst
import           System.FilePath                ( splitPath
                                                , joinPath
                                                , (</>)
                                                )
import           System.FilePath.Posix          ( takeDirectory )
import           System.Directory
import           System.IO.Error
import           System.Posix.Files
import qualified Streaming.Prelude             as Stream
import qualified Streaming.Conduit             as Conduit
import qualified Streaming.Zip                 as Stream

import           Constants
import           Foundation
import           Lib.ClientManifest
import           Lib.Error
import qualified Lib.External.AppMgr           as AppMgr
import           Lib.External.Registry
import           Lib.Sound
import           Lib.Ssl
import           Lib.Tor
import           Lib.Types.Core
import           Lib.Types.NetAddress
import           Lib.Types.Emver
import           Lib.SystemCtl
import           Lib.SystemPaths         hiding ( (</>) )
import           Settings
import           Util.File
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Daemon.ZeroConf                ( getStart9AgentHostname )
import qualified Data.Text                     as T
import           Control.Effect.Error    hiding ( run )


data Synchronizer = Synchronizer
    { synchronizerVersion    :: Version
    , synchronizerOperations :: [SyncOp]
    }

data SyncOp = SyncOp
    { syncOpName           :: Text
    , syncOpShouldRun      :: ReaderT AgentCtx IO Bool -- emit true if op is to be run
    , syncOpRun            :: ReaderT AgentCtx IO ()
    , syncOpRequiresReboot :: Bool
    }

data Arch = ArmV7 | ArmV8 deriving (Show)
data KernelVersion = KernelVersion
    { kernelVersionNumber :: Version
    , kernelVersionArch   :: Arch
    }
    deriving Show

parseKernelVersion :: Parser KernelVersion
parseKernelVersion = do
    major' <- decimal
    minor' <- char '.' *> decimal
    patch' <- char '.' *> decimal
    arch   <- string "-v7l+" $> ArmV7 <|> string "-v8+" $> ArmV8
    pure $ KernelVersion (Version (major', minor', patch', 0)) arch

synchronizer :: Synchronizer
synchronizer = sync_0_2_6
{-# INLINE synchronizer #-}

sync_0_2_6 :: Synchronizer
sync_0_2_6 = Synchronizer
    "0.2.6"
    [ syncCreateAgentTmp
    , syncCreateSshDir
    , syncRemoveAvahiSystemdDependency
    , syncInstallAppMgr
    , syncFullUpgrade
    , sync32BitKernel
    , syncInstallNginx
    , syncWriteNginxConf
    , syncInstallDuplicity
    , syncInstallExfatFuse
    , syncInstallExfatUtils
    , syncInstallAmbassadorUI
    , syncOpenHttpPorts
    , syncUpgradeLifeline
    , syncPrepSslRootCaDir
    , syncPrepSslIntermediateCaDir
    , syncPersistLogs
    , syncConvertEcdsaCerts
    ]

syncCreateAgentTmp :: SyncOp
syncCreateAgentTmp = SyncOp "Create Agent Tmp Dir" check migrate False
    where
        check = do
            s   <- asks appSettings
            tmp <- injectFilesystemBaseFromContext s $ getAbsoluteLocationFor agentTmpDirectory
            liftIO $ not <$> doesPathExist (toS tmp)
        migrate = do
            s   <- asks appSettings
            tmp <- injectFilesystemBaseFromContext s $ getAbsoluteLocationFor agentTmpDirectory
            liftIO $ createDirectoryIfMissing True (toS tmp)

syncCreateSshDir :: SyncOp
syncCreateSshDir = SyncOp "Create SSH directory" check migrate False
    where
        check = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ not <$> doesPathExist (toS $ sshKeysDirectory `relativeTo` base)
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ createDirectoryIfMissing False (toS $ sshKeysDirectory `relativeTo` base)

syncRemoveAvahiSystemdDependency :: SyncOp
syncRemoveAvahiSystemdDependency = SyncOp "Remove Avahi Systemd Dependency" check migrate False
    where
        wanted = decodeUtf8 $(embedFile "config/agent.service")
        check  = do
            base    <- asks $ appFilesystemBase . appSettings
            content <- liftIO $ readFile (toS $ agentServicePath `relativeTo` base)
            pure (content /= wanted)
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ writeFile (toS $ agentServicePath `relativeTo` base) wanted
            void $ liftIO systemCtlDaemonReload

-- the main purpose of this is the kernel upgrade but it does upgrade all packages on the system, maybe we should
-- reconsider the heavy handed approach here
syncFullUpgrade :: SyncOp
syncFullUpgrade = SyncOp "Full Upgrade" check migrate True
    where
        check = liftIO . run $ do
            v <- decodeUtf8 <<$>> (uname ("-r" :: Text) $| conduit await)
            case parse parseKernelVersion <$> v of
                Just (Done _ (KernelVersion (Version av) _)) -> if av < (4, 19, 118, 0) then pure True else pure False
                _ -> pure False
        migrate = liftIO . run $ do
            shell "apt update"
            shell "apt full-upgrade -y"

sync32BitKernel :: SyncOp
sync32BitKernel = SyncOp "32 Bit Kernel Switch" check migrate True
    where
        getBootCfgPath = getAbsoluteLocationFor bootConfigPath
        check          = do
            settings <- asks appSettings
            cfg      <- injectFilesystemBaseFromContext settings getBootCfgPath
            liftIO . run $ isNothing <$> (shell [i|grep "arm_64bit=0" #{cfg} || true|] $| conduit await)
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            let tmpFile = bootConfigTempPath `relativeTo` base
            let bootCfg = bootConfigPath `relativeTo` base
            contents <- liftIO $ readFile (toS bootCfg)
            let contents' = unlines . (<> ["arm_64bit=0"]) . filter (/= "arm_64bit=1") . lines $ contents
            liftIO $ writeFile (toS tmpFile) contents'
            liftIO $ renameFile (toS tmpFile) (toS bootCfg)

syncInstallNginx :: SyncOp
syncInstallNginx = SyncOp "Install Nginx" check migrate False
    where
        check   = liftIO . run $ fmap isNothing (shell [i|which nginx || true|] $| conduit await)
        migrate = liftIO . run $ do
            apt "update"
            apt "install" "nginx" "-y"

syncInstallDuplicity :: SyncOp
syncInstallDuplicity = SyncOp "Install duplicity" check migrate False
    where
        check   = liftIO . run $ fmap isNothing (shell [i|which duplicity || true|] $| conduit await)
        migrate = liftIO . run $ do
            apt "update"
            apt "install" "-y" "duplicity"

syncInstallExfatFuse :: SyncOp
syncInstallExfatFuse = SyncOp "Install exfat-fuse" check migrate False
    where
        check =
            liftIO
                $       (run (shell [i|dpkg -l|] $| shell [i|grep exfat-fuse|] $| conduit await) $> False)
                `catch` \(e :: ProcessException) -> case e of
                            ProcessException _ (ExitFailure 1) -> pure True
                            _ -> throwIO e
        migrate = liftIO . run $ do
            apt "update"
            apt "install" "-y" "exfat-fuse"

syncInstallExfatUtils :: SyncOp
syncInstallExfatUtils = SyncOp "Install exfat-utils" check migrate False
    where
        check =
            liftIO
                $       (run (shell [i|dpkg -l|] $| shell [i|grep exfat-utils|] $| conduit await) $> False)
                `catch` \(e :: ProcessException) -> case e of
                            ProcessException _ (ExitFailure 1) -> pure True
                            _ -> throwIO e
        migrate = liftIO . run $ do
            apt "update"
            apt "install" "-y" "exfat-utils"

syncWriteConf :: Text -> ByteString -> SystemPath -> SyncOp
syncWriteConf name contents' confLocation = SyncOp [i|Write #{name} Conf|] check migrate False
    where
        contents = decodeUtf8 contents'
        check    = do
            base <- asks $ appFilesystemBase . appSettings
            conf <-
                liftIO
                $       (Just <$> readFile (toS $ confLocation `relativeTo` base))
                `catch` (\(e :: IOException) -> if isDoesNotExistError e then pure Nothing else throwIO e)
            pure $ case conf of
                Nothing -> True
                Just co -> co /= contents
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            void . liftIO $ createDirectoryIfMissing True (takeDirectory (toS $ confLocation `relativeTo` base))
            liftIO $ writeFile (toS $ confLocation `relativeTo` base) contents

syncPrepSslRootCaDir :: SyncOp
syncPrepSslRootCaDir = SyncOp "Create Embassy Root CA Environment" check migrate False
    where
        check = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO . fmap not . doesPathExist . toS $ rootCaDirectory `relativeTo` base
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ do
                createDirectoryIfMissing True . toS $ rootCaDirectory `relativeTo` base
                for_ ["/certs", "/crl", "/newcerts", "/private"] $ \p -> do
                    createDirectoryIfMissing True . toS $ p `relativeTo` (rootCaDirectory `relativeTo` base)
                setFileMode (toS $ (rootCaDirectory <> "/private") `relativeTo` base) (7 `shiftL` 6)
                writeFile (toS $ (rootCaDirectory <> "/index.txt") `relativeTo` base) ""
                writeFile (toS $ (rootCaDirectory <> "/serial") `relativeTo` base)    "1000"
                BS.writeFile (toS $ rootCaOpenSslConfPath `relativeTo` base)
                             (root_CA_OPENSSL_CONF . toS $ rootCaDirectory `relativeTo` base)

syncPrepSslIntermediateCaDir :: SyncOp
syncPrepSslIntermediateCaDir = SyncOp "Create Embassy Intermediate CA Environment" check migrate False
    where
        check = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO . fmap not . doesPathExist . toS $ intermediateCaDirectory `relativeTo` base
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ do
                createDirectoryIfMissing True . toS $ intermediateCaDirectory `relativeTo` base
                for_ ["/certs", "/crl", "/newcerts", "/private"] $ \p -> do
                    createDirectoryIfMissing True . toS $ (intermediateCaDirectory <> p) `relativeTo` base
                setFileMode (toS $ (intermediateCaDirectory <> "/private") `relativeTo` base) (7 `shiftL` 6)
                writeFile (toS $ (intermediateCaDirectory <> "/index.txt") `relativeTo` base) ""
                writeFile (toS $ (intermediateCaDirectory <> "/serial") `relativeTo` base)    "1000"
                BS.writeFile (toS $ intermediateCaOpenSslConfPath `relativeTo` base)
                             (intermediate_CA_OPENSSL_CONF . toS $ intermediateCaDirectory `relativeTo` base)

syncWriteNginxConf :: SyncOp
syncWriteNginxConf = syncWriteConf "Nginx" $(embedFile "config/nginx.conf") nginxConfig

syncInstallAmbassadorUI :: SyncOp
syncInstallAmbassadorUI = SyncOp "Install Ambassador UI" check migrate False
    where
        check = do
            base <- asks (appFilesystemBase . appSettings)
            liftIO (doesPathExist (toS $ ambassadorUiPath `relativeTo` base)) >>= \case
                True -> do
                    manifest <- liftIO $ readFile (toS $ ambassadorUiManifestPath `relativeTo` base)
                    case Yaml.decodeEither' (encodeUtf8 manifest) of
                        Left  _            -> pure False
                        Right (Some1 _ cm) -> case cm of
                            (V0 cmv0) -> pure $ clientManifestV0AppVersion cmv0 /= agentVersion
                False -> pure True
        migrate = mapReaderT runResourceT $ do
            base <- asks (appFilesystemBase . appSettings)
            liftIO $ removePathForcibly (toS $ ambassadorUiPath `relativeTo` base)

            void
                . runInContext
                -- untar and save to path
                $ streamUntar (ambassadorUiPath `relativeTo` base)
                -- unzip
                . Stream.gunzip
                -- download
                $ getAmbassadorUiForSpec (exactly agentVersion)

            runM $ injectFilesystemBase base $ do
                -- if the ssl config has already been setup, we want to override the config with new UI details
                -- otherwise we leave it alone
                whenM (liftIO $ doesFileExist (toS $ nginxSitesAvailable nginxSslConf `relativeTo` base)) $ do
                    sid <- getStart9AgentHostname
                    let hostname = sid <> ".local"
                    installAmbassadorUiNginxHTTPS
                        (NginxSiteConfOverride
                            hostname
                            443
                            (Just $ NginxSsl { nginxSslKeyPath         = entityKeyPath sid
                                             , nginxSslCertPath        = entityCertPath sid
                                             , nginxSslOnlyServerNames = [hostname]
                                             }
                            )
                        )
                        nginxSslConf
                installAmbassadorUiNginxHTTP nginxTorConf

        streamUntar :: (MonadResource m, MonadThrow m) => Text -> ByteStream.ByteStream m () -> m ()
        streamUntar root stream = Conduit.runConduit $ Conduit.fromBStream stream .| Conduit.untar \f -> do
            let path = toS . (toS root </>) . joinPath . drop 1 . splitPath . B8.unpack . Conduit.filePath $ f
            print path
            if Conduit.fileType f == Conduit.FTDirectory
                then liftIO $ createDirectoryIfMissing True path
                else Conduit.sinkFile path

installAmbassadorUiNginxHTTP :: (MonadIO m, HasFilesystemBase sig m) => SystemPath -> m ()
installAmbassadorUiNginxHTTP = installAmbassadorUiNginx Nothing

installAmbassadorUiNginxHTTPS :: (MonadIO m, HasFilesystemBase sig m) => NginxSiteConfOverride -> SystemPath -> m ()
installAmbassadorUiNginxHTTPS o = installAmbassadorUiNginx $ Just o

-- Private. Installs an nginx conf from client-manifest to 'fileName' and restarts nginx.
installAmbassadorUiNginx :: (MonadIO m, HasFilesystemBase sig m)
                         => Maybe NginxSiteConfOverride
                         -> SystemPath -- nginx conf file name
                         -> m ()
installAmbassadorUiNginx mSslOverrides fileName = do
    base   <- Fused.ask @"filesystemBase"

    -- parse app manifest
    -- generate nginx conf from app manifest
    -- write conf to ambassador target location
    appEnv <- flip runReaderT base . handleS9ErrNuclear $ liftA2
        (HM.intersectionWith (,))
        (AppMgr2.runAppMgrCliC $ HM.mapMaybe AppMgr2.infoResTorAddress <$> AppMgr2.list [AppMgr2.flags| |])
        AppMgr.readLanIps -- TODO: get appmgr to expose this or guarantee its structure
    agentTor <- getAgentHiddenServiceUrl
    let fullEnv = HM.insert (AppId "start9-ambassador") (TorAddress agentTor, LanIp "127.0.0.1") appEnv

    removeFileIfExists $ nginxAvailableConf base
    removeFileIfExists $ nginxEnabledConf base

    flip runReaderT fullEnv
        $   transpile mSslOverrides (ambassadorUiClientManifiest base) (nginxAvailableConf base)
        >>= \case
                True  -> pure ()
                False -> throwIO . InternalS9Error $ "Failed to write ambassador ui nginx config " <> show fileName
    liftIO $ createSymbolicLink (nginxAvailableConf base) (nginxEnabledConf base)

    -- restart nginx
    void . liftIO $ systemCtl RestartService "nginx"
    where
        ambassadorUiClientManifiest b = toS $ (ambassadorUiPath <> "/client-manifest.yaml") `relativeTo` b
        nginxAvailableConf b = toS $ nginxSitesAvailable fileName `relativeTo` b
        nginxEnabledConf b = toS $ nginxSitesEnabled fileName `relativeTo` b

syncOpenHttpPorts :: SyncOp
syncOpenHttpPorts = SyncOp "Open Hidden Service Port 80" check migrate False
    where
        check = runResourceT $ do
            base <- asks $ appFilesystemBase . appSettings
            res  <-
                ByteStream.readFile (toS $ AppMgr.torrcBase `relativeTo` base)
                & ByteStream.lines
                & Stream.mapped ByteStream.toStrict
                & Stream.map decodeUtf8
                & Stream.filter
                      ( (    (== ["HiddenServicePort", "443", "127.0.0.1:443"])
                        <||> (== ["HiddenServicePort", "80", "127.0.0.1:80"])
                        )
                      . words
                      )
                & Stream.toList_
            if length res < 2 then pure True else pure False
        migrate = cantFail . flip catchE failUpdate $ do
            lift $ syncOpRun $ syncWriteConf "Torrc" $(embedFile "config/torrc") AppMgr.torrcBase
            AppMgr.torReload

syncInstallAppMgr :: SyncOp
syncInstallAppMgr = SyncOp "Install AppMgr" check migrate False
    where
        check = runExceptT AppMgr.getAppMgrVersion >>= \case
            Left  _ -> pure True
            Right v -> not . (v <||) <$> asks (appMgrVersionSpec . appSettings)
        migrate = fmap (either absurd id) . runExceptT . flip catchE failUpdate $ do
            avs <- asks $ appMgrVersionSpec . appSettings
            av  <- AppMgr.installNewAppMgr avs
            unless (av <|| avs) $ throwE $ AppMgrVersionE av avs

syncUpgradeLifeline :: SyncOp
syncUpgradeLifeline = SyncOp "Upgrade Lifeline" check migrate False
    where
        clearResets :: SystemPath
        clearResets = "/usr/local/bin/clear-resets.sh"
        check       = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ doesFileExist . toS $ clearResets `relativeTo` base
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            removeFileIfExists . toS $ lifelineBinaryPath `relativeTo` base
            mapReaderT runResourceT $ runInContext $ getLifelineBinary (exactly "0.2.0")
            removeFileIfExists . toS $ clearResets `relativeTo` base

syncPersistLogs :: SyncOp
syncPersistLogs =
    (syncWriteConf "Journald" $(embedFile "config/journald.conf") journaldConfig) { syncOpRequiresReboot = True }

syncRepairSsl :: SyncOp
syncRepairSsl = SyncOp "Repair SSL Certs" check migrate False
    where
        check = do
            base <- asks $ appFilesystemBase . appSettings
            let p = toS $ sslDirectory `relativeTo` base
            liftIO $ not <$> doesDirectoryExist p
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            let newCerts = toS $ (agentTmpDirectory <> sslDirectory) `relativeTo` base
            liftIO $ renameDirectory newCerts (toS $ sslDirectory `relativeTo` base)
            liftIO $ systemCtl RestartService "nginx" $> ()

syncConvertEcdsaCerts :: SyncOp
syncConvertEcdsaCerts = SyncOp "Convert Intermediate Cert to ECDSA P256" check migrate False
    where
        check = do
            fs     <- asks $ appFilesystemBase . appSettings
            header <- liftIO $ headMay . lines <$> readFile (toS $ intermediateCaKeyPath `relativeTo` fs)
            pure $ case header of
                Nothing -> False
                Just y  -> "BEGIN RSA PRIVATE KEY" `T.isInfixOf` y
        migrate = cantFail $ do
            base <- asks $ appFilesystemBase . appSettings
            (runM . runExceptT) (injectFilesystemBase base replaceDerivativeCerts) >>= \case
                Left  e  -> failUpdate e
                Right () -> pure ()


replaceDerivativeCerts :: (HasFilesystemBase sig m, Fused.Has (Error S9Error) sig m, MonadIO m) => m ()
replaceDerivativeCerts = do
    hn             <- getStart9AgentHostname
    tor            <- getAgentHiddenServiceUrl

    caKeyPath      <- toS <$> getAbsoluteLocationFor rootCaKeyPath
    caConfPath     <- toS <$> getAbsoluteLocationFor rootCaOpenSslConfPath
    caCertPath     <- toS <$> getAbsoluteLocationFor rootCaCertPath

    intCaKeyPath   <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
    intCaConfPath  <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
    intCaCertPath  <- toS <$> getAbsoluteLocationFor intermediateCaCertPath

    sslDirTmp      <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> sslDirectory)
    entKeyPathTmp  <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityKeyPath hn)
    entConfPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityConfPath hn)
    entCertPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityCertPath hn)
    liftIO $ createDirectoryIfMissing True sslDirTmp
    liftIO $ BS.writeFile entConfPathTmp (domain_CSR_CONF hn)

    -- ensure duplicate certificates are acceptable
    base <- Fused.ask @"filesystemBase"
    liftIO $ BS.writeFile (toS $ (rootCaDirectory <> "index.txt.attr") `relativeTo` base) "unique_subject = no\n"
    liftIO $ BS.writeFile (toS $ (intermediateCaDirectory <> "index.txt.attr") `relativeTo` base)
                          "unique_subject = no\n"

    (ec, out, err) <- writeIntermediateCert DeriveCertificate { applicantConfPath = intCaConfPath
                                                              , applicantKeyPath  = intCaKeyPath
                                                              , applicantCertPath = intCaCertPath
                                                              , signingConfPath   = caConfPath
                                                              , signingKeyPath    = caKeyPath
                                                              , signingCertPath   = caCertPath
                                                              , duration          = 3650
                                                              }
    liftIO $ do
        putStrLn @Text "openssl logs"
        putStrLn @Text "exit code: "
        print ec
        putStrLn @String $ "stdout: " <> out
        putStrLn @String $ "stderr: " <> err
    case ec of
        ExitSuccess   -> pure ()
        ExitFailure n -> throwError $ OpenSslE "leaf" n out err

    (ec', out', err') <- writeLeafCert
        DeriveCertificate { applicantConfPath = entConfPathTmp
                          , applicantKeyPath  = entKeyPathTmp
                          , applicantCertPath = entCertPathTmp
                          , signingConfPath   = intCaConfPath
                          , signingKeyPath    = intCaKeyPath
                          , signingCertPath   = intCaCertPath
                          , duration          = 365
                          }
        hn
        tor
    liftIO $ do
        putStrLn @Text "openssl logs"
        putStrLn @Text "exit code: "
        print ec
        putStrLn @String $ "stdout: " <> out'
        putStrLn @String $ "stderr: " <> err'
    case ec' of
        ExitSuccess   -> pure ()
        ExitFailure n -> throwError $ OpenSslE "leaf" n out' err'

    sslDir <- toS <$> getAbsoluteLocationFor sslDirectory
    liftIO $ removePathForcibly sslDir
    liftIO $ renameDirectory sslDirTmp sslDir
    liftIO $ systemCtl RestartService "nginx" $> ()

failUpdate :: S9Error -> ExceptT Void (ReaderT AgentCtx IO) ()
failUpdate e = do
    ref <- asks appIsUpdateFailed
    putStrLn $ "UPDATE FAILED: " <> errorMessage (toError e)
    liftIO $ playSong 216 beethoven
    liftIO $ writeIORef ref (Just e)

cantFail :: Monad m => ExceptT Void m a -> m a
cantFail = fmap (either absurd id) . runExceptT
