{-# LANGUAGE QuasiQuotes #-}
module Daemon.SslRenew where

import           Startlude               hiding ( err )

import           Data.String.Interpolate        ( i )
import           System.Process                 ( system )

import           Constants
import           Control.Carrier.Lift
import           Daemon.ZeroConf                ( getStart9AgentHostname )
import qualified Data.ByteString               as BS
import           Database.Persist.Sql           ( Filter
                                                , SqlPersistT
                                                , count
                                                , runSqlPool
                                                )
import           Foundation
import qualified Lib.Notifications             as Notifications
import           Lib.Ssl
import           Lib.SystemCtl
import           Lib.SystemPaths
import           Lib.Tor
import           Lib.Types.Core
import           Model
import           Settings
import           System.Directory               ( createDirectoryIfMissing
                                                , doesPathExist
                                                , removePathForcibly
                                                , renameDirectory
                                                )
import           System.FilePath                ( takeDirectory )

renewSslLeafCert :: AgentCtx -> IO ()
renewSslLeafCert ctx = do
    let base = appFilesystemBase . appSettings $ ctx
    sid <- injectFilesystemBase base getStart9AgentHostname
    let hostname = sid <> ".local"
    tor <- injectFilesystemBase base getAgentHiddenServiceUrl
    putStr @Text "SSL Renewal Required? "
    needsRenew <- flip runSqlPool (appConnPool ctx) $ doesSslNeedRenew (toS $ entityCertPath sid `relativeTo` base)
    print needsRenew
    when needsRenew $ runM . injectFilesystemBase base $ do
        intCaKeyPath   <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
        intCaConfPath  <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
        intCaCertPath  <- toS <$> getAbsoluteLocationFor intermediateCaCertPath

        sslDirTmp      <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> sslDirectory)
        entKeyPathTmp  <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityKeyPath sid)
        entConfPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityConfPath sid)
        entCertPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityCertPath sid)

        liftIO $ createDirectoryIfMissing True sslDirTmp
        liftIO $ BS.writeFile entConfPathTmp (domain_CSR_CONF hostname)

        (ec, out, err) <- writeLeafCert
            DeriveCertificate { applicantConfPath = entConfPathTmp
                              , applicantKeyPath  = entKeyPathTmp
                              , applicantCertPath = entCertPathTmp
                              , signingConfPath   = intCaConfPath
                              , signingKeyPath    = intCaKeyPath
                              , signingCertPath   = intCaCertPath
                              , duration          = 365
                              }
            hostname
            tor
        liftIO $ do
            putStrLn @Text "openssl logs"
            putStrLn @Text "exit code: "
            print ec
            putStrLn @String $ "stdout: " <> out
            putStrLn @String $ "stderr: " <> err
        case ec of
            ExitFailure n ->
                liftIO
                    . void
                    $ flip runSqlPool (appConnPool ctx)
                    $ Notifications.emit (AppId "EmbassyOS") agentVersion
                    $ Notifications.CertRenewFailed (ExitFailure n) out err
            ExitSuccess -> liftIO $ do
                let sslDir = toS $ sslDirectory `relativeTo` base
                createDirectoryIfMissing True (takeDirectory sslDir)
                removePathForcibly sslDir
                renameDirectory sslDirTmp sslDir
                systemCtl RestartService "nginx" $> ()


doesSslNeedRenew :: FilePath -> SqlPersistT IO Bool
doesSslNeedRenew cert = do
    exists <- liftIO $ doesPathExist cert
    if exists
        then do
            ec <- liftIO $ system [i|openssl x509 -checkend 2592000 -noout -in #{cert}|]
            pure $ ec /= ExitSuccess
        else do
            -- if we have set up the embassy already, then this is bad state that needs to be repaired
            n <- count ([] :: [Filter Account])
            pure $ n >= 1
