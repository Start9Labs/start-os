{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Register.Nginx where

import           Startlude               hiding ( ask
                                                , catchError
                                                , err
                                                )

import           Control.Carrier.Error.Church
import           Control.Effect.Lift
import qualified Control.Effect.Reader.Labelled
                                               as Fused
import qualified Data.ByteString               as BS
import           System.Directory
import           Daemon.ZeroConf
import           Lib.ClientManifest
import           Lib.Error
import           Lib.Ssl
import           Lib.Synchronizers
import           Lib.SystemPaths
import           Lib.Tor
import           System.Posix                   ( removeLink )

-- Left error, Right CA cert for hmac signing
bootupSslNginx :: (HasFilesystemBase sig m, Has (Error S9Error) sig m, Has (Lift IO) sig m, MonadIO m)
               => ByteString
               -> m Text
bootupSslNginx rsaKeyFileContents = do
    -- we need to ensure if the ssl setup fails that we remove all openssl key material and the nginx ssl conf before
    -- starting again
    resetSslState
    cert <- writeSslKeyAndCert rsaKeyFileContents
    sid  <- getStart9AgentHostname
    installAmbassadorUiNginxHTTPS (sslOverrides sid) "start9-ambassador-ssl.conf"
    pure cert
    where
        sslOverrides sid =
            let hostname = sid <> ".local"
            in  NginxSiteConfOverride
                    { nginxSiteConfOverrideAdditionalServerName = hostname
                    , nginxSiteConfOverrideListen = 443
                    , nginxSiteConfOverrideSsl = Just $ NginxSsl { nginxSslKeyPath         = entityKeyPath sid
                                                                 , nginxSslCertPath        = entityCertPath sid
                                                                 , nginxSslOnlyServerNames = [hostname]
                                                                 }
                    }

resetSslState :: (HasFilesystemBase sig m, Has (Lift IO) sig m, MonadIO m) => m ()
resetSslState = do
    base <- Fused.ask @"filesystemBase"
    host <- getStart9AgentHostname
    -- remove all files we explicitly create
    traverse_
        (liftIO . removePathForcibly . toS . flip relativeTo base)
        [ rootCaKeyPath
        , relBase $ (rootCaCertPath `relativeTo` base) <> ".csr"
        , rootCaCertPath
        , intermediateCaKeyPath
        , relBase $ (intermediateCaCertPath `relativeTo` base) <> ".csr"
        , intermediateCaCertPath
        , entityKeyPath host
        , relBase $ (entityCertPath host `relativeTo` base) <> ".csr"
        , entityCertPath host
        , entityConfPath host
        , nginxSitesAvailable nginxSslConf
        ]
    liftIO $ do
        withCurrentDirectory (toS $ flip relativeTo base $ rootCaDirectory <> "/newcerts")
            $   listDirectory "."
            >>= traverse_ removePathForcibly
        withCurrentDirectory (toS $ flip relativeTo base $ intermediateCaDirectory <> "/newcerts")
            $   listDirectory "."
            >>= traverse_ removePathForcibly
        writeFile (toS $ flip relativeTo base $ rootCaDirectory <> "/index.txt")         ""
        writeFile (toS $ flip relativeTo base $ intermediateCaDirectory <> "/index.txt") ""
    _ <- liftIO $ try @SomeException . removeLink . toS $ nginxSitesEnabled nginxSslConf `relativeTo` base
    pure ()

bootupHttpNginx :: (HasFilesystemBase sig m, MonadIO m) => m ()
bootupHttpNginx = installAmbassadorUiNginxHTTP "start9-ambassador.conf"

writeSslKeyAndCert :: (MonadIO m, HasFilesystemBase sig m, Has (Error S9Error) sig m) => ByteString -> m Text
writeSslKeyAndCert rsaKeyFileContents = do
    directory     <- toS <$> getAbsoluteLocationFor sslDirectory
    caKeyPath     <- toS <$> getAbsoluteLocationFor rootCaKeyPath
    caConfPath    <- toS <$> getAbsoluteLocationFor rootCaOpenSslConfPath
    caCertPath    <- toS <$> getAbsoluteLocationFor rootCaCertPath
    intCaKeyPath  <- toS <$> getAbsoluteLocationFor intermediateCaKeyPath
    intCaConfPath <- toS <$> getAbsoluteLocationFor intermediateCaOpenSslConfPath
    intCaCertPath <- toS <$> getAbsoluteLocationFor intermediateCaCertPath
    sid           <- getStart9AgentHostname
    entKeyPath    <- toS <$> getAbsoluteLocationFor (entityKeyPath sid)
    entConfPath   <- toS <$> getAbsoluteLocationFor (entityConfPath sid)
    entCertPath   <- toS <$> getAbsoluteLocationFor (entityCertPath sid)
    torAddr       <- getAgentHiddenServiceUrl

    let hostname = sid <> ".local"

    liftIO $ createDirectoryIfMissing False directory
    liftIO $ BS.writeFile caKeyPath rsaKeyFileContents

    (exit, str1, str2) <- writeRootCaCert caConfPath caKeyPath caCertPath
    liftIO $ do
        putStrLn @Text "openssl logs"
        putStrLn @Text "exit code: "
        print exit
        putStrLn @String $ "stdout: " <> str1
        putStrLn @String $ "stderr: " <> str2
    case exit of
        ExitSuccess    -> pure ()
        ExitFailure ec -> throwError $ OpenSslE "root" ec str1 str2

    (exit', str1', str2') <- writeIntermediateCert $ DeriveCertificate { applicantConfPath = intCaConfPath
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
        print exit'
        putStrLn @String $ "stdout: " <> str1'
        putStrLn @String $ "stderr: " <> str2'
    case exit' of
        ExitSuccess    -> pure ()
        ExitFailure ec -> throwError $ OpenSslE "intermediate" ec str1' str2'


    liftIO $ BS.writeFile entConfPath (domain_CSR_CONF hostname)

    (exit'', str1'', str2'') <- writeLeafCert
        DeriveCertificate { applicantConfPath = entConfPath
                          , applicantKeyPath  = entKeyPath
                          , applicantCertPath = entCertPath
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
        print exit''
        putStrLn @String $ "stdout: " <> str1''
        putStrLn @String $ "stderr: " <> str2''
    case exit'' of
        ExitSuccess    -> pure ()
        ExitFailure ec -> throwError $ OpenSslE "leaf" ec str1' str2'

    readSystemPath' rootCaCertPath
