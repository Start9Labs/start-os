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
import           System.Directory               ( renameDirectory
                                                , removeDirectory
                                                )
import           Lib.SystemCtl
import qualified Lib.Notifications             as Notifications
import           Database.Persist.Sql           ( runSqlPool )
import           Lib.Types.Core
import           Constants

renewSslLeafCert :: AgentCtx -> IO ()
renewSslLeafCert ctx = do
    let base = appFilesystemBase . appSettings $ ctx
    hn  <- injectFilesystemBase base getStart9AgentHostname
    tor <- injectFilesystemBase base getAgentHiddenServiceUrl
    putStr @Text "SSL Renewal Required? "
    needsRenew <- doesSslNeedRenew (toS $ entityCertPath hn `relativeTo` base)
    print needsRenew
    when needsRenew $ runM . injectFilesystemBase base $ do
        intCaKeyPath   <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
        intCaConfPath  <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
        intCaCertPath  <- toS <$> getAbsoluteLocationFor intermediateCaCertPath

        sslDirTmp      <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> sslDirectory)
        entKeyPathTmp  <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityKeyPath hn)
        entConfPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityConfPath hn)
        entCertPathTmp <- toS <$> getAbsoluteLocationFor (agentTmpDirectory <> entityCertPath hn)

        (ec, out, err) <- writeLeafCert
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
        liftIO $ removeDirectory sslDir
        liftIO $ renameDirectory sslDirTmp sslDir
        liftIO $ systemCtl RestartService "nginx" $> ()


doesSslNeedRenew :: FilePath -> IO Bool
doesSslNeedRenew cert = do
    ec <- liftIO $ system [i|openssl x509 -checkend 2592000 -noout -in #{cert}|]
    pure $ ec /= ExitSuccess
