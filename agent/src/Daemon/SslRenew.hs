{-# LANGUAGE QuasiQuotes #-}
module Daemon.SslRenew where

import           Startlude               hiding ( err )

import           Data.String.Interpolate        ( i )
import           System.Process                 ( system )

import           Foundation
import           Lib.SystemPaths
import           Settings
import           Lib.Ssl
import           Daemon.ZeroConf                ( getStart9AgentHostname )
import           Lib.Tor
import           Control.Carrier.Lift
import           System.Directory               ( doesPathExist
                                                , removePathForcibly
                                                , renameDirectory
                                                )
import           Lib.SystemCtl
import qualified Lib.Notifications             as Notifications
import           Database.Persist.Sql           ( runSqlPool )
import           Lib.Types.Core
import           Constants

renewSslLeafCert :: AgentCtx -> IO ()
renewSslLeafCert ctx = do
    let base = appFilesystemBase . appSettings $ ctx
    sid <- injectFilesystemBase base getStart9AgentHostname
    let hostname = sid <> ".local"
    tor <- injectFilesystemBase base getAgentHiddenServiceUrl
    putStr @Text "SSL Renewal Required? "
    needsRenew <- doesSslNeedRenew (toS $ entityCertPath sid `relativeTo` base)
    print needsRenew
    when needsRenew $ runM . injectFilesystemBase base $ do
        intCaKeyPath   <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
        intCaConfPath  <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
        intCaCertPath  <- toS <$> getAbsoluteLocationFor intermediateCaCertPath

        sslDirTmp      <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> sslDirectory)
        entKeyPathTmp  <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityKeyPath sid)
        entConfPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityConfPath sid)
        entCertPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityCertPath sid)

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
            ExitSuccess -> pure ()
            ExitFailure n ->
                liftIO
                    . void
                    $ flip runSqlPool (appConnPool ctx)
                    $ Notifications.emit (AppId "EmbassyOS") agentVersion
                    $ Notifications.CertRenewFailed (ExitFailure n) out err
        let sslDir = toS $ sslDirectory `relativeTo` base
        liftIO $ removePathForcibly sslDir
        liftIO $ renameDirectory sslDirTmp sslDir
        liftIO $ systemCtl RestartService "nginx" $> ()


doesSslNeedRenew :: FilePath -> IO Bool
doesSslNeedRenew cert = do
    exists <- doesPathExist cert
    if exists
        then do
            ec <- liftIO $ system [i|openssl x509 -checkend 2592000 -noout -in #{cert}|]
            pure $ ec /= ExitSuccess
        else pure False
