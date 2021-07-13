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

import           Control.Carrier.Lift           ( runM )
import qualified Control.Effect.Reader.Labelled
                                               as Fused
import           Control.Monad.Trans.Reader     ( mapReaderT )
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B8
import qualified Data.Conduit                  as Conduit
import qualified Data.Conduit.Combinators      as Conduit
import           Data.Conduit.Shell      hiding ( arch
                                                , hostname
                                                , patch
                                                , split
                                                , stream
                                                )
import qualified Data.Conduit.Tar              as Conduit
import           Data.FileEmbed
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.String.Interpolate.IsString
import qualified Data.Yaml                     as Yaml
import           Exinst
import qualified Streaming.Conduit             as Conduit
import qualified Streaming.Prelude             as Stream
import qualified Streaming.Zip                 as Stream
import           System.Directory
import           System.FilePath                ( (</>)
                                                , joinPath
                                                , splitPath
                                                )
import           System.FilePath.Posix          ( takeDirectory )
import           System.IO.Error
import           System.Posix.Files
import           System.Process                 ( callCommand )

import           Constants
import           Control.Effect.Error    hiding ( run )
import           Control.Effect.Labelled        ( runLabelled )
import           Daemon.ZeroConf                ( getStart9AgentHostname )
import           Data.ByteString.Char8          ( split )
import qualified Data.ByteString.Char8         as C8
import           Data.Conduit.List              ( consume )
import qualified Data.Text                     as T
import           Foundation
import           Handler.Network
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.ClientManifest
import           Lib.Error
import qualified Lib.External.AppMgr           as AppMgr
import           Lib.External.Registry
import           Lib.Sound
import           Lib.Ssl
import           Lib.SystemCtl
import           Lib.SystemPaths         hiding ( (</>) )
import           Lib.Tor
import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.NetAddress
import           Settings
import           Util.File


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
synchronizer = sync_0_2_14
{-# INLINE synchronizer #-}

sync_0_2_14 :: Synchronizer
sync_0_2_14 = Synchronizer
    "0.2.14"
    [ syncCreateAgentTmp
    , syncCreateSshDir
    , syncRemoveAvahiSystemdDependency
    , syncInstallLibAvahi
    , syncInstallAppMgr
    , syncFullUpgrade
    , sync32BitKernel
    , syncInstallNginx
    , syncWriteNginxConf
    , syncInstallDuplicity
    , syncInstallExfatFuse
    , syncInstallExfatUtils
    , syncUpgradeTor
    , syncInstallAmbassadorUI
    , syncOpenHttpPorts
    , syncUpgradeLifeline
    , syncPrepSslRootCaDir
    , syncPrepSslIntermediateCaDir
    , syncPersistLogs
    , syncConvertEcdsaCerts
    , syncRestarterService
    , syncInstallEject
    , syncDropCertificateUniqueness
    , syncRemoveDefaultNginxCfg
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
            shell "apt-get update"
            shell "apt-get full-upgrade -y"

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
            shell "apt-get update"
            shell "apt-get install nginx -y"

syncInstallEject :: SyncOp
syncInstallEject = SyncOp "Install Eject" check migrate False
    where
        check   = liftIO . run $ fmap isNothing (shell [i|which eject || true|] $| conduit await)
        migrate = liftIO . run $ do
            shell "apt-get update"
            shell "apt-get install eject -y"

syncInstallDuplicity :: SyncOp
syncInstallDuplicity = SyncOp "Install duplicity" check migrate False
    where
        check   = liftIO . run $ fmap isNothing (shell [i|which duplicity || true|] $| conduit await)
        migrate = liftIO . run $ do
            shell "apt-get update"
            shell "apt-get install -y duplicity"

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
            shell "apt-get update"
            shell "apt-get install -y exfat-fuse"

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
            shell "apt-get update"
            shell "apt-get install -y exfat-utils"

syncInstallLibAvahi :: SyncOp
syncInstallLibAvahi = SyncOp "Install libavahi-client" check migrate False
    where
        check =
            liftIO
                $       (run (shell [i|dpkg -l|] $| shell [i|grep libavahi-client3|] $| conduit await) $> False)
                `catch` \(e :: ProcessException) -> case e of
                            ProcessException _ (ExitFailure 1) -> pure True
                            _ -> throwIO e
        migrate = liftIO . run $ do
            shell "apt-get update"
            shell "apt-get install -y libavahi-client3"

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
            lan <- asks appLanThread
            avs <- asks $ appMgrVersionSpec . appSettings
            av  <- AppMgr.installNewAppMgr avs
            unless (av <|| avs) $ throwE $ AppMgrVersionE av avs
            flip runReaderT lan $ runLabelled @"lanThread" $ postResetLanLogic -- to accommodate 0.2.x -> 0.2.9 where previous appmgr didn't correctly set up lan

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
            fs <- asks $ appFilesystemBase . appSettings
            let intCertKey = toS $ intermediateCaKeyPath `relativeTo` fs
            exists <- liftIO $ doesPathExist intCertKey
            if exists
                then do
                    header <- liftIO $ headMay . lines <$> readFile intCertKey
                    pure $ case header of
                        Nothing -> False
                        Just y  -> "BEGIN RSA PRIVATE KEY" `T.isInfixOf` y
                else pure False
        migrate = cantFail $ do
            base <- asks $ appFilesystemBase . appSettings
            (runM . runExceptT) (injectFilesystemBase base replaceDerivativeCerts) >>= \case
                Left  e  -> failUpdate e
                Right () -> pure ()


replaceDerivativeCerts :: (HasFilesystemBase sig m, Fused.Has (Error S9Error) sig m, MonadIO m) => m ()
replaceDerivativeCerts = do
    sid <- getStart9AgentHostname
    let hostname = sid <> ".local"
    torAddr        <- getAgentHiddenServiceUrl

    caKeyPath      <- toS <$> getAbsoluteLocationFor rootCaKeyPath
    caConfPath     <- toS <$> getAbsoluteLocationFor rootCaOpenSslConfPath
    caCertPath     <- toS <$> getAbsoluteLocationFor rootCaCertPath

    intCaKeyPath   <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
    intCaConfPath  <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
    intCaCertPath  <- toS <$> getAbsoluteLocationFor intermediateCaCertPath

    sslDirTmp      <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> sslDirectory)
    entKeyPathTmp  <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityKeyPath sid)
    entConfPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityConfPath sid)
    entCertPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityCertPath sid)
    liftIO $ createDirectoryIfMissing True sslDirTmp
    liftIO $ BS.writeFile entConfPathTmp (domain_CSR_CONF hostname)

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
        hostname
        torAddr
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

syncRestarterService :: SyncOp
syncRestarterService = SyncOp "Install Restarter Service" check migrate True
    where
        wantedService = $(embedFile "config/restarter.service")
        wantedTimer   = $(embedFile "config/restarter.timer")
        check         = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ not <$> doesPathExist
                (toS $ "/etc/systemd/system/timers.target.wants/restarter.timer" `relativeTo` base)
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ BS.writeFile (toS $ "/etc/systemd/system/restarter.service" `relativeTo` base) wantedService
            liftIO $ BS.writeFile (toS $ "/etc/systemd/system/restarter.timer" `relativeTo` base) wantedTimer
            liftIO $ callCommand "systemctl enable restarter.service"
            liftIO $ callCommand "systemctl enable restarter.timer"

syncUpgradeTor :: SyncOp
syncUpgradeTor = SyncOp "Install Latest Tor" check migrate False
    where
        check = run $ do
            shell "apt-get clean"
            shell "apt-get update"
            mTorVersion <- (shell "dpkg -s tor" $| shell "grep '^Version'" $| shell "cut -d ' ' -f2" $| conduit await)
            echo ("CURRENT TOR VERSION:" :: Text) (show mTorVersion :: Text)
            let torVersion = case mTorVersion of
                    Nothing -> panic "invalid output from dpkg, can't read tor version"
                    Just x  -> x
            pure $ compareTorVersions torVersion "0.3.5.15-1" == LT
        migrate = liftIO . run $ do
            shell "apt-get update"
            availVersions <-
                (shell "apt-cache madison tor" $| shell "cut -d '|' -f2" $| shell "xargs" $| conduit consume)
            let latest = case lastMay $ sortBy compareTorVersions availVersions of
                    Nothing -> panic "No available versions of tor"
                    Just x  -> x
            shell $ "apt-get install -y tor=" <> if "0.3.5.15-1" `elem` availVersions
                then "0.3.5.15-1"
                else (C8.unpack latest)
        compareTorVersions :: ByteString -> ByteString -> Ordering
        compareTorVersions a b =
            let a' = (traverse (readMaybe @Int . decodeUtf8) . (split '.' <=< split '-') $ a)
                b' = (traverse (readMaybe @Int . decodeUtf8) . (split '.' <=< split '-') $ b)
            in  case liftA2 compare a' b' of
                    Nothing -> panic "invalid tor version string"
                    Just x  -> x


syncDropCertificateUniqueness :: SyncOp
syncDropCertificateUniqueness = SyncOp "Eliminate OpenSSL unique_subject=yes" check migrate False
    where
        uni   = "unique_subject = no\n"
        check = do
            base         <- asks $ appFilesystemBase . appSettings
            contentsRoot <-
                liftIO
                $       (fmap Just . BS.readFile . toS $ (rootCaDirectory <> "index.txt.attr") `relativeTo` base)
                `catch` \(e :: IOException) -> if isDoesNotExistError e then pure Nothing else throwIO e
            contentsInt <-
                liftIO
                $ (fmap Just . BS.readFile . toS $ (intermediateCaDirectory <> "index.txt.attr") `relativeTo` base)
                `catch` \(e :: IOException) -> if isDoesNotExistError e then pure Nothing else throwIO e
            case (contentsRoot, contentsInt) of
                (Just root, Just int) -> pure $ uni /= root || uni /= int
                _                     -> pure True
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ BS.writeFile (toS $ (rootCaDirectory <> "index.txt.attr") `relativeTo` base) uni
            liftIO $ BS.writeFile (toS $ (intermediateCaDirectory <> "index.txt.attr") `relativeTo` base) uni

syncRemoveDefaultNginxCfg :: SyncOp
syncRemoveDefaultNginxCfg = SyncOp "Remove Default Nginx Configuration" check migrate False
    where
        check = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ doesPathExist (toS $ nginxSitesEnabled "default" `relativeTo` base)
        migrate = do
            base <- asks $ appFilesystemBase . appSettings
            liftIO $ removeFileIfExists (toS $ nginxSitesEnabled "default" `relativeTo` base)
            liftIO $ systemCtl RestartService "nginx" $> ()

failUpdate :: S9Error -> ExceptT Void (ReaderT AgentCtx IO) ()
failUpdate e = do
    ref <- asks appIsUpdateFailed
    putStrLn $ "UPDATE FAILED: " <> errorMessage (toError e)
    liftIO $ playSong 216 beethoven
    liftIO $ writeIORef ref (Just e)

cantFail :: Monad m => ExceptT Void m a -> m a
cantFail = fmap (either absurd id) . runExceptT
