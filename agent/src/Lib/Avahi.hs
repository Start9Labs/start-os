{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Avahi where

import           Startlude               hiding ( (<.>) )

import           Data.String.Interpolate.IsString
import qualified Data.Text                     as T
import           System.Directory

import           Lib.Error
import           Lib.SystemCtl
import           Lib.SystemPaths
import           Settings

avahiConf :: Text -> Text
avahiConf hostname = T.drop 1 $ [i|
[server]
host-name=#{hostname}
domain-name=local
use-ipv4=yes
use-ipv6=no
allow-interfaces=wlan0,eth0
ratelimit-interval-usec=100000
ratelimit-burst=1000

[wide-area]
enable-wide-area=yes

[publish]

[reflector]

[rlimits]
|]

data WildcardReplacement =
      WildcardsEnabled
    | WildcardsDisabled
    deriving (Eq, Show)

serviceConfig :: (WildcardReplacement, Text) -> Text -> Word16 -> Text
serviceConfig (wildcards, name) protocol port = T.drop 1 $ [i|
<?xml version="1.0" standalone='no'?><!--*-nxml-*-->
<!DOCTYPE service-group SYSTEM "avahi-service.dtd">
<service-group>
    <name replace-wildcards=#{show $ bool ("no" :: Text) "yes" (wildcards == WildcardsEnabled) :: Text}>#{name}</name>
    <service protocol="ipv4">
        <type>#{protocol}</type>
        <port>#{port}</port>
    </service>
</service-group>|]

createService :: (MonadReader AppSettings m, MonadIO m) => Text -> (WildcardReplacement, Text) -> Text -> Word16 -> m ()
createService title params proto port = do
    base <- asks appFilesystemBase
    liftIO $ writeFile (toS $ avahiServicePath title `relativeTo` base) $ serviceConfig params proto port

createDaemonConf :: Text -> IO ()
createDaemonConf = writeFile "/etc/avahi/avahi-daemon.conf" . avahiConf

listServices :: IO [FilePath]
listServices = listDirectory "/etc/avahi/services"

reload :: IO ()
reload = do
    ec <- systemCtl RestartService "avahi-daemon"
    unless (ec == ExitSuccess) $ throwIO . AvahiE $ "systemctl restart avahi-daemon" <> show ec
