module Handler.Notifications where

import           Startlude

import           Data.UUID
import           Database.Persist
import           Yesod.Core.Handler
import           Yesod.Core.Types               ( JSONResponse(..) )
import           Yesod.Persist.Core

import           Foundation
import qualified Lib.Notifications             as Notification
import           Model

getNotificationsR :: Handler (JSONResponse [Entity Notification])
getNotificationsR = runDB $ do
    page     <- lookupGetParam "page" `orDefaultTo` 1
    pageSize <- lookupGetParam "perPage" `orDefaultTo` 20
    evs      <- selectList [] [Desc NotificationCreatedAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
    let toArchive = fmap entityKey $ filter ((== Nothing) . notificationArchivedAt . entityVal) evs
    void $ Notification.archive toArchive
    pure $ JSONResponse evs
    where
        orDefaultTo :: (Monad m, Read a) => m (Maybe Text) -> a -> m a
        orDefaultTo m a = do
            m' <- m
            case m' >>= readMaybe . toS of
                Nothing -> pure a
                Just x  -> pure x

deleteNotificationR :: UUID -> Handler ()
deleteNotificationR notifId = runDB $ delete (coerce @_ @(Key Notification) notifId)
