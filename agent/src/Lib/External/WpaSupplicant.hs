{-# LANGUAGE QuasiQuotes #-}
module Lib.External.WpaSupplicant where

import           Startlude

import           Data.Bitraversable
import qualified Data.HashMap.Strict           as HM
import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import           System.Process
import           Control.Concurrent.Async.Lifted
                                               as LAsync
import           Control.Monad.Trans.Control    ( MonadBaseControl )

runWlan0 :: ReaderT Text m a -> m a
runWlan0 = flip runReaderT "wlan0"

isConnectedToEthernet :: MonadIO m => m Bool
isConnectedToEthernet = do
    liftIO $ not . null . filter (T.isInfixOf "inet ") . lines . toS <$> readProcess "ifconfig" ["eth0"] ""

-- There be bug here: if you're in the US, and add a network in Sweden, you'll set your wpa supplicant to be looking for networks in Sweden.
-- so you won't be autoconnecting to anything in the US till you add another US guy.
addNetwork :: MonadIO m => Text -> Text -> Text -> ReaderT Interface m ()
addNetwork ssid psk country = do
    interface <- ask
    networkId <- checkNetwork ssid >>= \case
        -- If the network already exists, we will update its password.
        Just nId -> do
            void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "new_password", toS nId, [i|"#{psk}"|]] ""
            pure nId

        -- Otherwise we create the network in the wpa_supplicant
        Nothing -> do
            nId <- liftIO $ T.strip . toS <$> readProcess "wpa_cli" ["-i", toS interface, "add_network"] ""
            void . liftIO $ readProcess "wpa_cli"
                                        ["-i", toS interface, "set_network", toS nId, "ssid", [i|"#{ssid}"|]]
                                        ""
            void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "set_network", toS nId, "psk", [i|"#{psk}"|]] ""
            void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "set_network", toS nId, "scan_ssid", "1"] ""
            pure nId

    void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "set", "country", toS country] ""
    void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "enable_network", toS networkId] ""
    void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "save_config"] ""

removeNetwork :: MonadIO m => Text -> ReaderT Interface m ()
removeNetwork ssid = do
    interface <- ask
    checkNetwork ssid >>= \case
        Nothing -> pure ()
        Just x  -> liftIO $ do
            void $ readProcess "wpa_cli" ["-i", toS interface, "remove_network", [i|#{x}|]] ""
            void $ readProcess "wpa_cli" ["-i", toS interface, "save_config"] ""
            void $ readProcess "wpa_cli" ["-i", toS interface, "reconfigure"] ""

listNetworks :: MonadIO m => ReaderT Interface m [Text]
listNetworks = do
    interface <- ask
    liftIO $ mapMaybe (`atMay` 1) . drop 1 . fmap (T.splitOn "\t") . lines . toS <$> readProcess
        "wpa_cli"
        ["-i", toS interface, "list_networks"]
        ""

type Interface = Text
getCurrentNetwork :: (MonadBaseControl IO m, MonadIO m) => ReaderT Interface m (Maybe Text)
getCurrentNetwork = do
    interface <- ask @Text
    liftIO $ guarded (/= "") . T.init . toS <$> readProcess "iwgetid" [toS interface, "--raw"] ""

selectNetwork :: (MonadBaseControl IO m, MonadIO m) => Text -> Text -> ReaderT Interface m Bool
selectNetwork ssid country = checkNetwork ssid >>= \case
    Nothing  -> putStrLn @Text "SSID Not Found" *> pure False
    Just nId -> do
        interface <- ask
        void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "select_network", toS nId] ""
        void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "set", "country", toS country] ""
        void . liftIO $ readProcess "wpa_cli" ["-i", toS interface, "save_config"] ""
        mNew <- join . hush <$> LAsync.race (liftIO $ threadDelay 20_000_000)
                                            (runMaybeT . asum $ repeat (MaybeT getCurrentNetwork))
        listNetworks >>= \nets ->
            for_ nets $ \net -> liftIO $ readProcess "wpa_cli" ["-i", toS interface, "enable_network", toS net] ""
        pure $ case mNew of
            Nothing         -> False
            Just newCurrent -> newCurrent == ssid

type NetworkId = Text
checkNetwork :: MonadIO m => Text -> ReaderT Interface m (Maybe NetworkId)
checkNetwork ssid = do
    interface <- ask
    HM.lookup ssid
        .   HM.fromList
        .   mapMaybe (bisequenceA . ((`atMay` 1) &&& (`atMay` 0)))
        .   drop 1
        .   fmap (T.splitOn "\t")
        .   lines
        .   toS
        <$> liftIO (readProcess "wpa_cli" ["-i", toS interface, "list_networks"] "")

-- TODO: Live Testing in GHCI
runWpa :: ReaderT Interface m a -> m a
runWpa = flip runReaderT "wlp5s0"
