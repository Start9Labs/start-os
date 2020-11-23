{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Register.Tor where

import           Startlude               hiding ( ask )

import           Control.Effect.Reader.Labelled
import qualified Data.ByteString               as BS
import           System.Directory
import           System.Process
import           Lib.SystemCtl
import           Lib.SystemPaths
import           Lib.Tor

bootupTor :: (HasFilesystemBase sig m, MonadIO m) => ByteString -> m (Maybe Text)
bootupTor torKeyFileContents = do
    base <- ask @"filesystemBase"
    writeTorPrivateKeyFile torKeyFileContents

    putStrLn @Text "restarting tor"
    liftIO . void $ systemCtl RestartService "tor"
    putStrLn @Text "restarted tor"

    liftIO . fmap (join . hush) $ race
        (threadDelay 30_000_000)
        (runMaybeT . asum . repeat $ MaybeT . fmap hush $ try @SomeException
            (threadDelay 100_000 *> injectFilesystemBase base getAgentHiddenServiceUrl)
        )

writeTorPrivateKeyFile :: (MonadIO m, HasFilesystemBase sig m) => ByteString -> m ()
writeTorPrivateKeyFile contents = do
    directory          <- fmap toS . getAbsoluteLocationFor $ agentTorHiddenServiceDirectory
    privateKeyFilePath <- fmap toS . getAbsoluteLocationFor $ agentTorHiddenServicePrivateKeyPath
    liftIO $ do
        -- Clean out directory
        removePathForcibly directory
        createDirectory directory

        -- write private key file
        BS.writeFile privateKeyFilePath contents

        -- Set ownership and permissions so tor executable can generate other files
        callCommand $ "chown -R debian-tor:debian-tor " <> directory
        callCommand $ "chmod 2700 " <> directory