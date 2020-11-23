module Handler.PowerOff where

import           Startlude

import           System.Process

import           Foundation
import           Lib.Sound
import           Yesod.Core.Handler
import           Network.HTTP.Types

postShutdownR :: Handler ()
postShutdownR = do
    liftIO $ callCommand "/bin/sync"
    liftIO $ playSong 400 marioDeath
    void $ liftIO $ forkIO $ do
        threadDelay 1_000_000
        callCommand "/sbin/shutdown now"
    sendResponseStatus status200 ()

postRestartR :: Handler ()
postRestartR = do
    liftIO $ callCommand "/bin/sync"
    liftIO $ playSong 400 marioDeath
    void $ liftIO $ forkIO $ do
        threadDelay 1_000_000
        callCommand "/sbin/reboot"
    sendResponseStatus status200 ()