{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Daemon.AppNotifications where

import           Startlude

import qualified Data.HashMap.Strict           as HM
import           Data.UUID.V4
import           Data.Time.Clock.POSIX
import           Database.Persist.Sql

import           Foundation
import           Lib.Error
import           Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.External.AppMgr           as AppMgr
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model

toModelNotif :: (AppId, Version) -> AppMgrNotif -> Notification
toModelNotif (appId, appVersion) AppMgrNotif {..} =
    let prefix = (<> "1") $ case appMgrNotifLevel of
            INFO    -> "0"
            SUCCESS -> "1"
            WARN    -> "2"
            ERROR   -> "3"
    in  Notification (posixSecondsToUTCTime . fromRational $ appMgrNotifTime)
                     Nothing
                     appId
                     appVersion
                     (prefix <> show appMgrNotifCode)
                     appMgrNotifTitle
                     appMgrNotifMessage

fetchAndSave :: ReaderT AgentCtx IO ()
fetchAndSave = handleErr $ do
    pool <- asks appConnPool
    apps <- HM.toList <$> AppMgr2.runAppMgrCliC (AppMgr2.list [AppMgr2.flags| |])
    for_ apps $ \(appId, AppMgr2.InfoRes { infoResVersion }) -> do
        notifs <- AppMgr.notifications appId
        let mods = toModelNotif (appId, infoResVersion) <$> notifs
        keys <- liftIO $ replicateM (length mods) (NotificationKey <$> nextRandom)
        let ents = zipWith Entity keys mods
        lift $ flip runSqlPool pool $ insertEntityMany ents
    where
        handleErr m = runExceptT m >>= \case
            Left  e -> putStrLn (errorMessage $ toError e)
            Right _ -> pure ()
